/*
 * (C) Copyright 2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "fv3jedi/GetValues/GetValues.h"

#include "oops/util/DateTime.h"
#include "oops/util/Logger.h"

#include "ufo/GeoVaLs.h"

#include "fv3jedi/Geometry/Geometry.h"
#include "fv3jedi/State/State.h"

namespace fv3jedi {

// -------------------------------------------------------------------------------------------------

GetValues::GetValues(const Geometry & geom, const ufo::Locations & locs,
                     const eckit::Configuration & conf)
  : locs_(locs), geom_(new Geometry(geom)) {
  oops::Log::trace() << "GetValues::GetValues starting" << std::endl;
  const eckit::Configuration * pconf = &conf;
  fv3jedi_getvalues_create_f90(keyGetValues_, geom.toFortran(), locs_, &pconf);
  oops::Log::trace() << "GetValues::GetValues done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

GetValues::~GetValues() {
  oops::Log::trace() << "GetValues::~GetValues starting" << std::endl;
  fv3jedi_getvalues_delete_f90(keyGetValues_);
  oops::Log::trace() << "GetValues::~GetValues done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

void GetValues::fillGeoVaLs(const State & state, const util::DateTime & t1,
                            const util::DateTime & t2, ufo::GeoVaLs & geovals) const {
  oops::Log::trace() << "GetValues::fillGeovals starting" << std::endl;
  fv3jedi_getvalues_fill_geovals_f90(keyGetValues_, geom_->toFortran(), state.toFortran(),
                                     t1, t2, locs_, geovals.toFortran());
  oops::Log::trace() << "GetValues::fillGeovals done" << std::endl;
}

// -------------------------------------------------------------------------------------------------

void GetValues::print(std::ostream & os) const {
  os << " GetValues for fv3-jedi" << std::endl;
}

// -------------------------------------------------------------------------------------------------

}  // namespace fv3jedi
