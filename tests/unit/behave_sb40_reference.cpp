// Optional reference generator, compiled against USFS firelab/behave commit
// f3b2943e69abca942e795c5b1b45276f6338abb5. No Behave dependency is required
// for the CFBM unit suite. See doc/Fuels.rst for compilation and interpretation.
#include "fuelModels.h"
#include "surfaceInputs.h"
#include "surfaceFuelbedIntermediates.h"
#include "surfaceFireReactionIntensity.h"
#include <iostream>
#include <iomanip>
int main() {
  FuelModels fuels;
  int codes[] = {101,106,121,141,145,149,161,163,164,181,188,204};
  for (int code : codes) for (double herb : {0.30,0.75,1.20,1.21}) {
    SurfaceInputs inputs;
    inputs.setMoistureInputMode(MoistureInputMode::BySizeClass);
    inputs.setMoistureOneHour(0.06,FractionUnits::Fraction);
    inputs.setMoistureTenHour(0.07,FractionUnits::Fraction);
    inputs.setMoistureHundredHour(0.08,FractionUnits::Fraction);
    inputs.setMoistureLiveHerbaceous(herb,FractionUnits::Fraction);
    inputs.setMoistureLiveWoody(0.90,FractionUnits::Fraction);
    SurfaceFuelbedIntermediates bed(fuels,inputs);
    bed.calculateFuelbedIntermediates(code);
    SurfaceFireReactionIntensity reaction(bed);
    double ir=reaction.calculateReactionIntensity();
    std::cout << std::setprecision(12) << code << " " << herb << " "
      << ir*bed.getPropagatingFlux()/bed.getHeatSink() << " "
      << reaction.getSurfaceFireReactionIntensityForLifeState(FuelLifeState::Dead) << " "
      << reaction.getSurfaceFireReactionIntensityForLifeState(FuelLifeState::Live) << " "
      << bed.getMoistureOfExtinctionByLifeState(FuelLifeState::Live) << "\n";
  }
}
