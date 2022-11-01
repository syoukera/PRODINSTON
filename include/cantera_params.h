
#pragma once

extern "C" {
  void getnextty(double y[], double *temperature);
  void getproperties(double y[], double *temperature, double diff[], double *lambda, double *cp);
}