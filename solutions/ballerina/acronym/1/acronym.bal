# Returns the acronym of the given phrase.
#
# + phrase - a string
# + return - the acronym
function abbreviate(string phrase) returns string {
    string acronym = "";
    string[] words = re`[-_ ]+`.split(phrase);
    foreach string word in words {
        acronym = acronym + word[0];
    }
    return acronym.toUpperAscii();
}
