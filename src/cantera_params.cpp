#include "cantera_params.h"
// include files for getnextty
#include "cantera/zerodim.h"
#include "cantera/thermo/IdealGasPhase.h"
#include "cantera/numerics/Integrator.h"
#include "example_utils.h"

// include files for getproperties
#include "cantera/base/Solution.h"
#include "cantera/transport.h"

using namespace Cantera;
using std::cout;
using std::endl;

void getnextty(double y[], double *temperature, double *dt)
{
    // cout << "\n**** Get next TY ****\n" << endl;

    // create an ideal gas mixture that corresponds to OH submech from GRI-Mech 3.0
    auto sol = newSolution("h2o2.yaml", "ohmech", "None");
    auto gas = sol->thermo();

    // set the state
    gas->setState_TPY(*temperature, OneAtm, y);
    int nsp = gas->nSpecies();

    // create a reactor
    IdealGasConstPressureReactor r;

    // 'insert' the gas into the reactor and environment.  Note
    // that it is ok to insert the same gas object into multiple
    // reactors or reservoirs. All this means is that this object
    // will be used to evaluate thermodynamic or kinetic
    // quantities needed.
    r.insert(sol);

    // double dt = 1.e+1; // interval at which output is written
    // int nsteps = 100; // number of intervals

    // create a 2D array to hold the output variables,
    // and store the values for the initial state
    Array2D soln(nsp+4, 1);
    saveSoln(0, 0.0, *(sol->thermo()), soln);

    // create a container object to run the simulation
    // and add the reactor to it
    ReactorNet sim;
    sim.addReactor(r);
    sim.advance(*dt);

    *temperature = r.temperature();
    for (size_t k = 0; k < nsp; k++) {
        y[k] = r.massFraction(k);
    }
}

void getproperties(double y[], double *temperature, double diff[], double *lambda, double *cp)
{
    // cout << "\n**** Get proparties ****\n" << endl;

    // create an ideal gas mixture that corresponds to OH submech from GRI-Mech 3.0
    auto sol = newSolution("h2o2.yaml", "ohmech", "None");

    // get class for themochemical properties
    auto gas = sol->thermo();

    // get number of species
    int nsp = gas->nSpecies();

    // set the state
    gas->setState_TPY(*temperature, OneAtm, y);

    // get mean specific heat in mass unit [J/kg/K]
    *cp = gas->cp_mass();
    
    // convert to CHEMKIN unit [Erg/g/K]
    *cp *= 1e4;
    
    // get class for obtaining transport properties
    std::unique_ptr<Transport> tr(newTransportMgr("Mix", sol->thermo().get()));

    // get mixture thermal conductivity [W/m/K]
    *lambda = tr->thermalConductivity();

    // convert to CHEMKIN unit [Erg/cm/K/s]
    *lambda *= 1e5;

    // get mixture-averaged diffusion coefficients [m2/s]
    tr->getMixDiffCoeffs(&diff[0]);

    // convert to CHEMKIN unit [cm2/s]
    for (size_t k = 0; k < nsp; k++) {
        diff[k] *= 1e4;
    }
}