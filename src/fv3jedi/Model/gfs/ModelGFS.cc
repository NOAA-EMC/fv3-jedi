/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <vector>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"

#include "oops/util/abor1_cpp.h"
#include "oops/util/DateTime.h"
#include "oops/util/Logger.h"

#include "fv3jedi/Geometry/Geometry.h"
#include "fv3jedi/Model/gfs/ModelGFS.h"
#include "fv3jedi/ModelBias/ModelBias.h"
#include "fv3jedi/State/State.h"
#include "fv3jedi/Utilities/Utilities.h"

namespace fv3jedi {
// -------------------------------------------------------------------------------------------------
static oops::ModelMaker<Traits, ModelGFS> makermodel_("GFS");
// -------------------------------------------------------------------------------------------------
ModelGFS::ModelGFS(const Geometry & resol, const eckit::Configuration & mconf)
  : keyConfig_(0), tstep_(0), geom_(resol), vars_(mconf)
{
  oops::Log::trace() << "ModelGFS::ModelGFS starting" << std::endl;
  tstep_ = util::Duration(mconf.getString("tstep"));
  const eckit::Configuration * mconfc = &mconf;

  // Create local config for model_configure
  const eckit::LocalConfiguration gfsmconf(mconf, "model_configure");
  const eckit::Configuration * gfsmconfc = &gfsmconf;

  // Jedi run directory
  getcwd(jedidir_, 10000);

  // Switch to GFS run directory
  std::string gfsRunDir = mconf.getString("gfs_run_dir");
  strcpy(gfsdir_, gfsRunDir.c_str());
  chdir(gfsdir_);

  fv3jedi_gfs_create_f90(keyConfig_, geom_.toFortran(), &mconfc, &gfsmconfc);

  // Switch back to JEDI directory
  chdir(jedidir_);

  oops::Log::trace() << "ModelGFS::ModelGFS done" << std::endl;
}
// -------------------------------------------------------------------------------------------------
ModelGFS::~ModelGFS() {
  oops::Log::trace() << "ModelGFS::~ModelGFS starting" << std::endl;
  chdir(gfsdir_);
  fv3jedi_gfs_delete_f90(keyConfig_);
  chdir(jedidir_);
  oops::Log::trace() << "ModelGFS::~ModelGFS done" << std::endl;
}
// -------------------------------------------------------------------------------------------------
void ModelGFS::initialize(State & xx) const {
  oops::Log::trace() << "ModelGFS::initialize starting" << std::endl;
  chdir(gfsdir_);
  util::DateTime * dtp = &xx.validTime();
  fv3jedi_gfs_initialize_f90(keyConfig_, xx.toFortran(), &dtp);
  chdir(jedidir_);
  oops::Log::trace() << "ModelGFS::initialize done" << std::endl;
}
// -------------------------------------------------------------------------------------------------
void ModelGFS::step(State & xx, const ModelBias &) const {
  oops::Log::trace() << "ModelGFS::step starting" << std::endl;
  chdir(gfsdir_);
  util::DateTime * dtp = &xx.validTime();
  fv3jedi_gfs_step_f90(keyConfig_, xx.toFortran(), &dtp);
  chdir(jedidir_);
  xx.validTime() += tstep_;
  oops::Log::trace() << "ModelGFS::step done" << std::endl;
}
// -------------------------------------------------------------------------------------------------
void ModelGFS::finalize(State & xx) const {
  oops::Log::trace() << "ModelGFS::finalize starting" << std::endl;
  chdir(gfsdir_);
  util::DateTime * dtp = &xx.validTime();
  fv3jedi_gfs_finalize_f90(keyConfig_, xx.toFortran(), &dtp);
  chdir(jedidir_);
  oops::Log::trace() << "ModelGFS::finalize done" << std::endl;
}
// -------------------------------------------------------------------------------------------------
int ModelGFS::saveTrajectory(State & xx, const ModelBias &) const {
  ABORT("Model:GFS should not be used for the trajecotry");
}
// -------------------------------------------------------------------------------------------------
void ModelGFS::print(std::ostream & os) const {
  os << "ModelGFS::print not implemented";
}
// -------------------------------------------------------------------------------------------------
}  // namespace fv3jedi
