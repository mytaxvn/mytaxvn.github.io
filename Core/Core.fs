module Core

type IncomeSource = {
    totalIncome: int64
    beginMonth: int
    endMonth: int
    insuranceDeduction: int64
    otherDeduction: int64
    withhold: int64
}

type Dependent = {
    beginMonth: int
    endMonth: int
}

type Setting = {
    personalDeductionPerMonth: int64
    dependentDeductionPerMonth: int64
} with
    static member default2025 = {
        personalDeductionPerMonth = 11_000_000
        dependentDeductionPerMonth = 4_400_000
    }
    static member default2026 = {
        personalDeductionPerMonth = 15_500_000
        dependentDeductionPerMonth = 6_200_000
    }

type Profile = {
    sources: IncomeSource list
    dependents: Dependent list
}

type PayResult =
    | NothingToDo of int64
    | PayMore of int64
    | PayBack of int64

type TaxResult = {
    totalIncome: int64
    personalDeduction: int64
    dependentDeduction: int64
    insuranceDeduction: int64
    otherDeduction: int64
    taxedIncome: int64
    tax: int64
    withhold: int64
    payResult: PayResult
}

let private money = [
    5_000_000
    10_000_000
    18_000_000
    32_000_000
    52_000_000
    80_000_000
]

let private percent = [
    0.05
    0.10
    0.15
    0.20
    0.25
    0.30
    0.35
]

let private tax (amount: int64) (numMonth: int) =
    if (amount <= 0 || numMonth <= 0) then 0L
    else
        let n = float amount / float numMonth
        let rec loop acc i =
            let prev = if i = 0 then 0. else money[i - 1]

            if i >= money.Length || n <= money[i] then
                acc + (n - prev) * percent[i]
            else
                loop
                    (acc + (float money[i] - prev) * percent[i])
                    (i + 1)
        int64(loop 0. 0) * int64(numMonth)

module Seq =
    let count predicate xs =
        xs |> Seq.filter predicate |> Seq.length

let calculate (setting: Setting) (profile: Profile) =
    let totalIncome = profile.sources |> List.sumBy _.totalIncome

    let workingMonths =
        seq {
            for src in profile.sources do
                yield! seq { src.beginMonth .. src.endMonth }
        } |> Set.ofSeq

    let personalDeduction = int64 workingMonths.Count * setting.personalDeductionPerMonth

    let dependentDeduction =
        profile.dependents
        |> List.sumBy (fun dep ->
            seq { dep.beginMonth .. dep.endMonth }
            |> Seq.count workingMonths.Contains
            |> (fun n -> int64 n * setting.dependentDeductionPerMonth)
        )

    let insuranceDeduction = profile.sources |> List.sumBy _.insuranceDeduction
    let otherDeduction = profile.sources |> List.sumBy _.otherDeduction

    let taxedIncome =
        totalIncome
            - personalDeduction - dependentDeduction - insuranceDeduction - otherDeduction
        |> max 0

    let tax = tax taxedIncome workingMonths.Count

    let withhold = profile.sources |> List.sumBy _.withhold

    let t = tax - withhold

    let threshold = 50_000L

    let payResult =
        if t > threshold then PayMore t
        elif t >= 0L && t <= threshold then NothingToDo t
        else PayBack -t

    {
        totalIncome = totalIncome
        personalDeduction = personalDeduction
        dependentDeduction = dependentDeduction
        insuranceDeduction = insuranceDeduction
        otherDeduction = otherDeduction
        taxedIncome = taxedIncome
        tax = tax
        withhold = withhold
        payResult = payResult
    }
