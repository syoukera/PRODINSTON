
#pragma once

extern "C" {
  void getnextty(double y[], double *temperature, double *dt);
  void getproperties(double y[], double *temperature, double diff[], double *lambda, double *cp);
}