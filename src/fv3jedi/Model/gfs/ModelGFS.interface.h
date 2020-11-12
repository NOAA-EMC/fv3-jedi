/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3JEDI_MODEL_GFS_MODELGFS_INTERFACE_H_
#define FV3JEDI_MODEL_GFS_MODELGFS_INTERFACE_H_

#include "fv3jedi/Utilities/interface.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
  class Duration;
}

namespace fv3jedi {

extern "C" {

  void fv3jedi_gfs_create_f90(F90model &,
                              const F90geom &,
                              const eckit::Configuration * const *,
                              const eckit::Configuration * const *);

  void fv3jedi_gfs_delete_f90(F90model &);


  void fv3jedi_gfs_initialize_f90(const F90model &,
                                  const F90state &,
                                  util::DateTime * const *);

  void fv3jedi_gfs_step_f90(const F90model &,
                            const F90state &,
                            util::DateTime * const *);

  void fv3jedi_gfs_finalize_f90(const F90model &,
                                const F90inc &,
                                util::DateTime * const *);

}  // extern "C"
// -----------------------------------------------------------------------------

}  // namespace fv3jedi
#endif  // FV3JEDI_MODEL_GFS_MODELGFS_INTERFACE_H_
