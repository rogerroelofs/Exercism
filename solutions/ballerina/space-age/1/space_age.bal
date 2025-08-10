
const map<float> orbitalPeriods = {
    Mercury: 0.2408467,
    Venus: 0.61519726,
    Earth: 1.0,
    Mars: 1.8808158,
    Jupiter: 11.862615,
    Saturn: 29.447498,
    Uranus: 84.016846,
    Neptune: 164.79132
};

# Returns the age on another planet or error if it is not a planet.
#
# + planet - planet name as a string
# + seconds - person's age measured in seconds
# + return - person's age in years of that planet, or error if unknown planet.
function age(string planet, int seconds) returns float|error {
    if !orbitalPeriods.hasKey(planet) {
        return error("not a planet");
    }

    float earthYearInSec = 31557600.0;
    return (<float>seconds / orbitalPeriods.get(planet) / earthYearInSec).round(2);
}
