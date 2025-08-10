import ballerina/http;

service on new http:Listener(9090) {
    resource function post greeting(@http:Payload string payload) returns string|error {
        // Extract payload content and append it with the content of the greeting message.
        // The greeting message should be in the following format:
        // "Hello, <payload>!"
        return "Hello, " + payload + "!";
    }
}
