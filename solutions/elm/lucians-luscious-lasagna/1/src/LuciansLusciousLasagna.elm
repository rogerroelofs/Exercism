module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)


expectedMinutesInOven =
    40


minutesPerLayer =
    2


preparationTimeInMinutes layers =
    minutesPerLayer * layers


elapsedTimeInMinutes layers timeInOven =
    preparationTimeInMinutes layers + timeInOven
