/*
 * (C) Copyright 2017-2022 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "boost/none_t.hpp"

#include "atlas/field.h"
#include "atlas/functionspace.h"

#include "eckit/exception/Exceptions.h"

#include "oops/base/GeometryData.h"
#include "oops/base/Variables.h"
#include "oops/generic/GlobalInterpolator.h"
#include "oops/util/abor1_cpp.h"
#include "oops/util/DateTime.h"
#include "oops/util/Duration.h"
#include "oops/util/Logger.h"

#include "fv3jedi/Geometry/Geometry.h"
#include "fv3jedi/Increment/Increment.h"
#include "fv3jedi/IO/Utils/IOBase.h"
#include "fv3jedi/State/State.h"
#include "fv3jedi/VariableChange/VariableChange.h"

namespace fv3jedi {

// -------------------------------------------------------------------------------------------------

State::State(const Geometry & geom, const oops::Variables & vars, const util::DateTime & time)
  : geom_(geom),
    vars_(geom_.fieldsMetaData().getLongNameFromAnyName(vars)),
    varsJedi_(geom_.fieldsMetaData().removeInterfaceSpecificFields(vars)),
    time_(time)
{
  oops::Log::trace() << "State::State (from geom, vars and time) starting" << std::endl;
  fv3jedi_state_create_f90(keyState_, geom_.toFortran(), vars_, time_);
  oops::Log::trace() << "State::State (from geom, vars and time) done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

State::State(const Geometry & geom, const eckit::Configuration & config)
  : geom_(geom), vars_(), varsJedi_(), time_(util::DateTime())
{
  oops::Log::trace() << "State::State (from geom and parameters) starting" << std::endl;
  StateParameters params;
  params.deserialize(config);

  // Set up vars
  if (params.analytic.value() != boost::none) {
    // Variables are hard coded for analytic initial condition (must not be provided)
    ASSERT(params.stateVariables.value() == boost::none);
    vars_ = oops::Variables({"ua", "va", "t", "delp", "p", "sphum", "ice_wat", "liq_wat", "phis",
                             "o3mr", "w"});
  } else {
    // If variables are being read they must be defined in the config
    ASSERT(params.stateVariables.value() != boost::none);
    vars_ = oops::Variables(*params.stateVariables.value());
  }
  stdvars_ = vars_;  // The original "standard" names are required by NUOPC_Advertise

  // Set long name variables
  vars_ = geom_.fieldsMetaData().getLongNameFromAnyName(vars_);
  varsJedi_ = geom_.fieldsMetaData().removeInterfaceSpecificFields(vars_);

  // Datetime from the config for read and analytical
  ASSERT(params.datetime.value() != boost::none);
  time_ = util::DateTime(*params.datetime.value());

  // Datetime from the config for read and analytical
  ASSERT(params.datetime.value() != boost::none);
  time_ = util::DateTime(*params.datetime.value());

  // Allocate state
  fv3jedi_state_create_f90(keyState_, geom_.toFortran(), vars_, time_);

  // Generate analytical state or read from file
  if (params.analytic.value() != boost::none) {
    this->analytic_init(params.analytic.value()->toConfiguration(), geom);
  } else {
    this->read(params.toConfiguration());
  }

  oops::Log::trace() << "State::State (from geom and parameters) done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

State::State(const Geometry & resol, const State & other)
  : geom_(resol), vars_(other.vars_), varsJedi_(other.varsJedi_), time_(other.time_)
{
  oops::Log::trace() << "State::State (from geom and other) starting" << std::endl;
  fv3jedi_state_create_f90(keyState_, geom_.toFortran(), vars_, time_);
  this->changeResolution(other);
  oops::Log::trace() << "State::State (from geom and other) done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

State::State(const oops::Variables & vars, const State & other) : State(other)
{
  oops::Log::trace() << "State::State (from vars and other) starting" << std::endl;
  eckit::LocalConfiguration varChangeConfig;
  varChangeConfig.set("variable change name", "Analysis2Model");
  VariableChange an2model(varChangeConfig, geom_);
  an2model.changeVarInverse(*this, vars);
  oops::Log::trace() << "State::State (from vars and other) done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

State::State(const State & other)
  : geom_(other.geom_), vars_(other.vars_), varsJedi_(other.varsJedi_), time_(other.time_)
{
  oops::Log::trace() << "State::State (from other) starting" << std::endl;
  fv3jedi_state_create_f90(keyState_, geom_.toFortran(), vars_, time_);
  fv3jedi_state_copy_f90(keyState_, other.keyState_);
  oops::Log::trace() << "State::State (from other) done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

State::~State() {
  fv3jedi_state_delete_f90(keyState_);
}

// -------------------------------------------------------------------------------------------------

State & State::operator=(const State & rhs) {
  fv3jedi_state_copy_f90(keyState_, rhs.keyState_);
  time_ = rhs.time_;
  return *this;
}

// -------------------------------------------------------------------------------------------------

void State::changeResolution(const State & other) {
  // If both states have same resolution, then copy instead of interpolating
  if (geom_.isEqual(other.geom_)) {
    fv3jedi_state_copy_f90(keyState_, other.keyState_);
    time_ = other.time_;
    return;
  }

  // Build oops interpolator -- this takes a few extra steps at this level
  const oops::GeometryData source_geom(other.geom_.functionSpace(),
                                       other.geom_.fields(),
                                       other.geom_.levelsAreTopDown(),
                                       other.geom_.getComm());
  const atlas::FunctionSpace target_fs = geom_.functionSpace();
  eckit::LocalConfiguration conf;
  // Use oops interpolator to handle integer/categorical fields correctly
  // Once the atlas interpolator gains support for this feature, we could make this configurable
  // from the user-facing yaml file; for now though, the atlas interpolator would be wrong for the
  // many integer fields of fv3-jedi.
  conf.set("local interpolator type", "oops unstructured grid interpolator");
  oops::GlobalInterpolator interp(conf, source_geom, target_fs, geom_.getComm());

  atlas::FieldSet source{};
  atlas::FieldSet target{};

  // Interpolate atlas::FieldSet representation of fv3 data
  other.toFieldSet(source);
  interp.apply(source, target);
  this->fromFieldSet(target);

  // Interpolation did not act on interface fields
  this->setInterfaceFieldsOutOfDate(true);
}

// -------------------------------------------------------------------------------------------------

void State::updateFields(const oops::Variables & newVars) {
  const oops::Variables newLongVars = geom_.fieldsMetaData().getLongNameFromAnyName(newVars);
  vars_ = newLongVars;
  varsJedi_ = geom_.fieldsMetaData().removeInterfaceSpecificFields(newLongVars);
  fv3jedi_state_update_fields_f90(keyState_, geom_.toFortran(), vars_);
}

// -------------------------------------------------------------------------------------------------

State & State::operator+=(const Increment & dx) {
  ASSERT(this->validTime() == dx.validTime());
  // Increment variables must be a equal to or a subset of the State variables
  ASSERT(dx.variables() <= vars_);
  // Interpolate increment to state resolution
  Increment dx_sr(geom_, dx);
  // Make sure State's data representations are synchronized.
  // Note: empirically, this is not needed (as of Oct 2023) for Variational applications, but is
  // needed for EnsRecenter, because that adds an increment to an *interpolated* state.
  this->synchronizeInterfaceFields();
  // Call transform and add
  fv3jedi_state_add_increment_f90(keyState_, dx_sr.toFortran(), geom_.toFortran());
  return *this;
}

// -------------------------------------------------------------------------------------------------

void State::analytic_init(const eckit::Configuration & config, const Geometry & geom) {
  fv3jedi_state_analytic_init_f90(keyState_, geom.toFortran(), config);
}

// -------------------------------------------------------------------------------------------------

void State::read(const eckit::Configuration & config) {
  StateParameters params;
  params.deserialize(config);
  // Optionally set the datetime on read (needed for some bump applications)
  if (params.setdatetime.value() != boost::none) {
    if (*params.setdatetime.value() && params.datetime.value() != boost::none) {
      time_ = *params.datetime.value();
    }
  }
  IOBase_ io(IOFactory::create(geom_, *params.ioParametersWrapper.ioParameters.value()));
  io->read(*this);
}

// -------------------------------------------------------------------------------------------------

void State::write(const eckit::Configuration & config) const {
  StateWriteParameters params;
  params.deserialize(config);
  IOBase_ io(IOFactory::create(geom_, *params.ioParametersWrapper.ioParameters.value()));

  this->synchronizeInterfaceFields();
  io->write(*this);
}

// -------------------------------------------------------------------------------------------------

void State::print(std::ostream & os) const {
  // Get the number of fields
  int numberFields;
  int cubeSize;
  fv3jedi_state_getnfieldsncube_f90(keyState_, numberFields, cubeSize);

  // Header
  os << std::endl
     << "--------------------------------------------------"
        "--------------------------------------------------";
  os << std::endl << "State print | number of fields = " << numberFields
                  << " | cube sphere face size: C" << cubeSize;

  // Print info field by field
  const int FieldNameLen = 45;
  char fieldName[FieldNameLen];
  std::vector<double> minMaxRms(3);
  for (int f = 0; f < numberFields; f++) {
    int fp1 = f+1;
    fv3jedi_state_getminmaxrms_f90(keyState_, fp1, FieldNameLen-1, fieldName, minMaxRms[0]);
    std::string fieldNameStr(fieldName);
    os << std::endl << std::scientific << std::showpos << fieldNameStr.substr(0, FieldNameLen-1)
                    << " | Min:" << minMaxRms[0] << " Max:" << minMaxRms[1]
                    << " RMS:" << minMaxRms[2] << std::noshowpos;
  }

  os.unsetf(std::ios_base::floatfield);

  // Footer
  os << std::endl
     << "--------------------------------------------------"
        "--------------------------------------------------";
}

// -------------------------------------------------------------------------------------------------

void State::zero() {
  fv3jedi_state_zero_f90(keyState_);
}

// -------------------------------------------------------------------------------------------------

void State::accumul(const double & zz, const State & xx) {
  fv3jedi_state_axpy_f90(keyState_, zz, xx.keyState_);
}

// -------------------------------------------------------------------------------------------------

double State::norm() const {
  this->synchronizeInterfaceFields();
  double zz = 0.0;
  fv3jedi_state_norm_f90(keyState_, zz);
  return zz;
}

// -------------------------------------------------------------------------------------------------

void State::toFieldSet(atlas::FieldSet & fset) const {
  fv3jedi_state_to_fieldset_f90(keyState_, geom_.toFortran(), varsJedi_, fset.get());
}

// -------------------------------------------------------------------------------------------------

void State::fromFieldSet(const atlas::FieldSet & fset) {
  fv3jedi_state_from_fieldset_f90(keyState_, geom_.toFortran(), varsJedi_, fset.get());
}

// -------------------------------------------------------------------------------------------------

void State::synchronizeInterfaceFields() const {
  fv3jedi_state_synchronize_interface_fields_f90(keyState_, geom_.toFortran());
}

// -----------------------------------------------------------------------------

void State::setInterfaceFieldsOutOfDate(const bool outofdate) const {
  fv3jedi_state_set_interface_fields_outofdate_f90(keyState_, outofdate);
}

// -----------------------------------------------------------------------------

size_t State::serialSize() const {
  oops::Log::trace() << "State serialSize starting" << std::endl;
  size_t nn = 1;
  int sz = 0;
  fv3jedi_state_sersize_f90(keyState_, sz);
  nn += sz;
  nn += time_.serialSize();
  oops::Log::trace() << "State serialSize done" << std::endl;
  return nn;
}

// -------------------------------------------------------------------------------------------------

void State::serialize(std::vector<double> & vect) const {
  oops::Log::trace() << "State serialize starting" << std::endl;
  int size_fld = this->serialSize() - 3;
  std::vector<double> v_fld(size_fld, 0);

  fv3jedi_state_serialize_f90(keyState_, size_fld, v_fld.data());
  vect.insert(vect.end(), v_fld.begin(), v_fld.end());

  // Serialize the date and time
  vect.push_back(-54321.56789);
  time_.serialize(vect);

  oops::Log::trace() << "State serialize done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

void State::deserialize(const std::vector<double> & vect, size_t & index) {
  oops::Log::trace() << "State deserialize starting" << std::endl;
  fv3jedi_state_deserialize_f90(keyState_, vect.size(), vect.data(), index);

  ASSERT(vect.at(index) == -54321.56789);
  ++index;

  time_.deserialize(vect, index);
  oops::Log::trace() << "State deserialize done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

}  // namespace fv3jedi
