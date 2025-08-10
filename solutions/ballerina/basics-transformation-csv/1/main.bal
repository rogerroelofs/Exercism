import ballerina/io;
import ballerina/regex;

type FillUp record {|
    int employeeId;
    int odometer;
    decimal gallons;
    decimal pricePerGallon;
|};

type FillUpSummary record {|
    readonly int employeeId;
    int gasFillUpCount;
    decimal totalFuelCost;
    decimal totalGallons;
    int totalMilesAccrued;
|};
type Mileage record {|
    readonly int employeeId;
    int startingMileage;
|};

function processFuelRecords(string inputFilePath, string outputFilePath) returns error? {
    string[][] data = check io:fileReadCsv(inputFilePath);
    FillUp[] fillUps = [];
    foreach string[] row in data {
        fillUps.push(check createFillUp(row));
    }
    table<FillUpSummary> fillUpSummary = summarizeFillUps(fillUps);
    string[][] output = generateOutput(fillUpSummary);
    _ = check io:fileWriteCsv(outputFilePath, output);
}

function createFillUp(string[] row) returns FillUp|error {
    return <FillUp>{
        employeeId: check int:fromString(regex:replaceAll(row[0].trim(), "^0+(?!$)", "")),
        odometer: check int:fromString(regex:replaceAll(row[1].trim(), "^0+(?!$)", "")),
        gallons: check decimal:fromString(regex:replaceAll(row[2].trim(), "^0+(?!$)", "")),
        pricePerGallon: check decimal:fromString(regex:replaceAll(row[3].trim(), "^0+(?!$)", ""))
    };
}

function summarizeFillUps(FillUp[] fillUps) returns table<FillUpSummary> {
    table<FillUpSummary> key(employeeId) summary = table [];
    table<Mileage> key(employeeId) mileage = table [];
    foreach var fillUp in fillUps {
        if (summary.hasKey(fillUp.employeeId)) {
            FillUpSummary s = summary.get(fillUp.employeeId);
            Mileage m = mileage.get(fillUp.employeeId);
            summary.put(<FillUpSummary>{
                employeeId: fillUp.employeeId,
                gasFillUpCount: s.gasFillUpCount + 1,
                totalFuelCost: s.totalFuelCost + (fillUp.gallons * fillUp.pricePerGallon),
                totalGallons: s.totalGallons + fillUp.gallons,
                totalMilesAccrued: fillUp.odometer - m.startingMileage
            });
        } else {
            mileage.add(<Mileage>{
                employeeId: fillUp.employeeId,
                startingMileage: fillUp.odometer
            });
            summary.add(<FillUpSummary>{
                employeeId: fillUp.employeeId,
                gasFillUpCount: 1,
                totalFuelCost: (fillUp.gallons * fillUp.pricePerGallon),
                totalGallons: fillUp.gallons,
                totalMilesAccrued: 0
            });
        }
    }
    return summary;
}

function generateOutput(table<FillUpSummary> fillUpSummary) returns string[][] {
    string[][] output = from var row in fillUpSummary order by row.employeeId select [
        row.employeeId.toString(), 
        row.gasFillUpCount.toString(), 
        row.totalFuelCost.toString(), 
        row.totalGallons.toString(), 
        row.totalMilesAccrued.toString()
    ];
    return output;
}