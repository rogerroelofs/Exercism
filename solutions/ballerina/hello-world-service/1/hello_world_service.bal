import ballerina/http;

// Implement an HTTP service listening on port 9090 with the root path "/"
service on new http:Listener(9090) {

    // Add a GET resource called "greeting" that returns the "Hello, World!" message
    resource function get greeting() returns string {
        return "Hello, World!";
    }
}
