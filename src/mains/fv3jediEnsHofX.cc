/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "fv3jedi/Utilities/Traits.h"
#include "oops/runs/EnsHofX.h"
#include "oops/runs/Run.h"
#include "ufo/instantiateObsFilterFactory.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  ufo::instantiateObsFilterFactory<fv3jedi::Traits>();
  oops::EnsHofX<fv3jedi::Traits> enshofx;
  return run.execute(enshofx);
}
