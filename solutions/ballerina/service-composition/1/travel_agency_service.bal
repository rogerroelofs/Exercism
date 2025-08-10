// import ballerina/mime;
import ballerina/http;

// Client endpoint to communicate with Airline reservation service
final http:Client airlineReservationEP = check new ("http://localhost:9091/airline");

// Client endpoint to communicate with Hotel reservation service
final http:Client hotelReservationEP = check new ("http://localhost:9092/hotel");

// Client endpoint to communicate with Car rental service
final http:Client carRentalEP = check new ("http://localhost:9093/car");

// Travel agency service to arrange a complete tour for a user
service /travel on new http:Listener(9090) {

    // Define a resource method to arrange a tour, that accepts `POST` requests in the path `/arrangeTour`.
    // This resource should accept a value of the type `TourArrangement` that already defined below.
    resource function post arrangeTour(@http:Payload TourArrangement tour) returns http:StatusCodeResponse|error {

        // Extract Travel infomation from the travel reservation request
        var { name, arrivalDate, departureDate, preference } = tour;
        Reservation reservation = {
            name,
            arrivalDate,
            departureDate, 
            preference: preference.airline
        };
        json flight = check airlineReservationEP->post("/reserve", reservation);
        if flight.status == FAILED {
            return <http:BadRequest> {
                body: {message: "Failed to reserve airline! Provide a valid 'preference' for 'airline' and try again"}
            };
        }

        reservation.preference = preference.accomodation;
        json hotel = check hotelReservationEP->post("/reserve", reservation);
        if hotel.status == FAILED {
            return <http:BadRequest> {
                body: {message: "Failed to reserve hotel! Provide a valid 'preference' for 'accommodation' and try again"}
            };
        }

        reservation.preference = preference.car;
        json car = check carRentalEP->post("/rent", reservation);
        if car.status == FAILED {
            return <http:BadRequest> {
                body: {message: "Failed to rent car! Provide a valid 'preference' for 'car' and try again"}
            };
        }

        // If all three services response positive status, send a successful message to the user
        // with the payload {"message":"Congratulations! Your journey is ready!!"}
        // The status code of the response should be 201 Created
        return <http:Created> {
            body: {message: "Congratulations! Your journey is ready!!"}
        };
    }
}

# The payload type received from the tour arrangement service.
#
# + name - Name of the tourist
# + arrivalDate - The arrival date of the tourist
# + departureDate - The departure date of the tourist
# + preference - The preferences for the airline, hotel, and the car rental
type TourArrangement record {|
    string name;
    string arrivalDate;
    string departureDate;
    Preference preference;
|};

# The different prefenrences for the tour.
#
# + airline - The preference for airline ticket. Can be `First`, `Bussiness`, `Economy`
# + accomodation - The prefenerece for the hotel reservarion. Can be `delux` or `superior`
# + car - The preference for the car rental. Can be `air conditioned`, or `normal`
type Preference record {|
    string airline;
    string accomodation;
    string car;
|};

// Define a record type to send requests to the reservation services.
type Reservation record {|
    string name;
    string arrivalDate;
    string departureDate;
    string preference;
|};

// The response type received from the reservation services
type ServiceResponse record {|
    Status status;
|};

type Response record {|
    string message;
|};

// Possible statuses of the reservation service responses
enum Status {
    SUCCESS = "Success",
    FAILED = "Failed"
}
