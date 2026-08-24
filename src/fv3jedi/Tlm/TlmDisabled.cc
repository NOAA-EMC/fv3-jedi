/*
 * (C) Copyright 2026 NOAA
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "oops/util/abor1_cpp.h"

#include "fv3jedi/Tlm/Tlm.h"

namespace fv3jedi {

namespace {
const char * unavailableMessage = "FV3-JEDI-LM support was disabled at build time";
}

Tlm::Tlm(const Geometry & geom, const eckit::Configuration &)
  : geom_(geom), keySelf_(0), tstep_(), trajmap_()
{
  ABORT(unavailableMessage);
}

Tlm::~Tlm() = default;

void Tlm::setTrajectory(const State &, State &, const ModelBias &) {
  ABORT(unavailableMessage);
}

void Tlm::initializeTL(Increment &) const {
  ABORT(unavailableMessage);
}

void Tlm::stepTL(Increment &, const ModelBiasIncrement &) const {
  ABORT(unavailableMessage);
}

void Tlm::finalizeTL(Increment &) const {
  ABORT(unavailableMessage);
}

void Tlm::initializeAD(Increment &) const {
  ABORT(unavailableMessage);
}

void Tlm::stepAD(Increment &, ModelBiasIncrement &) const {
  ABORT(unavailableMessage);
}

void Tlm::finalizeAD(Increment &) const {
  ABORT(unavailableMessage);
}

void Tlm::print(std::ostream & os) const {
  os << unavailableMessage;
}

}  // namespace fv3jedi