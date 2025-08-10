// import ballerina/oauth2;
import ballerina/http;

# Add the necessary attributes to this record to accept two operands and an operator.
#
# + operand1 - Is a float used as the first operand in an equation
# + operand2 - Is a float used as the second operand in an equation
# + operator - Is a string that represents the operator
public type Calculation record {|
    float operand1;
    float operand2;
    string operator;
|};

# Add the necessary attributes to this record to include the result value and the expression.
#
# + result - The result of the operation
# + expression - The evaluated expression that used to calculate the result
public type Response record {|
    float result;
    string expression;
|};

service / on new http:Listener(9090) {

    // Add HTTP resource function to accept a POST request on path '/calc'
    // The function should accept the above Calculation record as the payload and return a generic json object
    resource function post calc(@http:Payload Calculation calc) returns Response {
        string operators = "+-*/";
        float op1 = calc.operand1;
        float op2 = calc.operand2;
        string op = calc.operator;
        float result = 0.0;
        // Check for each operator '+', '-', 'x' or '*' and '/'. and do the calculation
        match op {
            "+" => { result = op1 + op2; }
            "-" => { result = op1 - op2; }
            "*" => { result = op1 * op2; }
            "/" => { result = op1 / op2; }
        }
        // Convert the two operands and the expression into a string representation with no whitespace.
        string expression = op1.toString() + op + op2.toString();

        // Return the result as a Response with the calculation expressed as a string e.g. { result: 0.0, expression: "0+0" };
        Response resp = {result: result, expression: expression};
        return resp;
    }

}