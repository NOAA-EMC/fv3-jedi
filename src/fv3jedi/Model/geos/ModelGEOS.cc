/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <unistd.h>

#include <string>
#include <vector>

#include "eckit/config/Configuration.h"

#include "oops/base/ParameterTraitsVariables.h"
#include "oops/util/abor1_cpp.h"
#include "oops/util/DateTime.h"
#include "oops/util/Logger.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"
#include "oops/util/parameters/RequiredParameter.h"

#include "fv3jedi/Geometry/Geometry.h"
#include "fv3jedi/Model/geos/ModelGEOS.h"
#include "fv3jedi/ModelBias/ModelBias.h"
#include "fv3jedi/State/State.h"

namespace fv3jedi {
// -----------------------------------------------------------------------------
/// Options taken by ModelGEOS
class ModelGEOSParameters : public oops::ModelParametersBase {
  OOPS_CONCRETE_PARAMETERS(ModelGEOSParameters, ModelParametersBase)

 public:
  oops::RequiredParameter<std::string> geosRunDirectory{ "geos_run_directory", this};
  oops::RequiredParameter<util::Duration> tstep{ "tstep", this};

  oops::OptionalParameter<bool> reforecast{ "reforecast", this};
  oops::OptionalParameter<bool> esmfLogging{ "ESMF_Logging", this};
};
// -----------------------------------------------------------------------------
static oops::interface::ModelMaker<Traits, ModelGEOS> makermodel_("GEOS");
// -----------------------------------------------------------------------------
ModelGEOS::ModelGEOS(const Geometry & resol, const eckit::Configuration & config)
  : keyConfig_(0), tstep_(0), geom_(resol)
{
  oops::Log::trace() << "ModelGEOS::ModelGEOS" << std::endl;
  ModelGEOSParameters params;
  params.deserialize(config);

  tstep_ = util::Duration(config.getString("tstep"));

  // JEDI to GEOS directory
  getcwd(jedidir_, 10000);

  std::string sGEOSSCRDIR = params.geosRunDirectory.value();
  strcpy(geosscrdir_, sGEOSSCRDIR.c_str());
  chdir(geosscrdir_);

  // Create the model
  fv3jedi_geos_create_f90(config, geom_.toFortran(), keyConfig_);

  // GEOS to JEDI directory
  chdir(jedidir_);

  oops::Log::trace() << "ModelGEOS created" << std::endl;
}
// -----------------------------------------------------------------------------
ModelGEOS::~ModelGEOS() {
  chdir(geosscrdir_);
  fv3jedi_geos_delete_f90(keyConfig_);
  chdir(jedidir_);
  oops::Log::trace() << "ModelGEOS destructed" << std::endl;
}
// -----------------------------------------------------------------------------
void ModelGEOS::initialize(State & xx) const {
  chdir(geosscrdir_);
  fv3jedi_geos_initialize_f90(keyConfig_, xx.toFortran());
  chdir(jedidir_);
  oops::Log::trace() << "ModelGEOS::initialize" << std::endl;
}
// -----------------------------------------------------------------------------
void ModelGEOS::step(State & xx, const ModelBias &) const {
  oops::Log::trace() << "ModelGEOS::step starting" << xx.validTime() << std::endl;
  xx.validTime() += tstep_;
  chdir(geosscrdir_);
  fv3jedi_geos_step_f90(keyConfig_, xx.toFortran());
  chdir(jedidir_);
  oops::Log::trace() << "ModelGEOS::step done" << xx.validTime() << std::endl;
}
// -----------------------------------------------------------------------------
void ModelGEOS::finalize(State & xx) const {
  chdir(geosscrdir_);
  fv3jedi_geos_finalize_f90(keyConfig_, xx.toFortran());
  chdir(jedidir_);
  oops::Log::trace() << "ModelGEOS::finalize" << std::endl;
}
// -----------------------------------------------------------------------------
void ModelGEOS::print(std::ostream & os) const {
  os << "ModelGEOS::print not implemented";
}
// -----------------------------------------------------------------------------
}  // namespace fv3jedi
