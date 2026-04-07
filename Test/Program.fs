let profile: Core.Profile = {
    sources = [
        {
            totalIncome = 123_456_789
            beginMonth = 1
            endMonth = 12
            insuranceDeduction = 0
            otherDeduction = 0
            withhold = 0
        }
    ]
    dependents = [
        { beginMonth = 1; endMonth = 12 }
        { beginMonth = 1; endMonth = 12 }
        { beginMonth = 1; endMonth = 12 }
    ]
}

let setting = Core.Setting.default2025

let res = Core.calculate setting profile
printfn "%A" res
System.Console.WriteLine("{0:N}", res.pay)
