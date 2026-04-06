module Types

open System

type MoneyInput = Result<int64, string>

module MoneyInput =
    let zero : MoneyInput = Ok 0

    let parse (str: string) : MoneyInput =
        if String.IsNullOrWhiteSpace str then zero
        else
            let cleaned =
                str.Replace(".", "")
                    .Replace(",", "")
                    .Replace("_", "")
                    .Trim()
            match Int64.TryParse cleaned  with
            | true, n -> Ok n
            | _ -> Error str

type IncomeSource = {
    companyName: string
    totalIncome: MoneyInput
    beginMonth: int
    endMonth: int
    insuranceDeduction: MoneyInput
    otherDeduction: MoneyInput
    withhold: MoneyInput
} with
    static member defaultValue = {
        companyName = ""
        totalIncome = MoneyInput.zero
        beginMonth = 1
        endMonth = 12
        insuranceDeduction = MoneyInput.zero
        otherDeduction = MoneyInput.zero
        withhold = MoneyInput.zero
    }

type Dependent = {
    name: string
    beginMonth: int
    endMonth: int
} with
    static member defaultvalue = {
        name = ""
        beginMonth = 1
        endMonth = 12
    }

type Standard =
    | Std2025
    | Std2026

type Setting = {
    personalDeductionPerMonth: MoneyInput
    dependentDeductionPerMonth: MoneyInput
} with
    static member create (core: Core.Setting) = {
        personalDeductionPerMonth = Ok core.personalDeductionPerMonth
        dependentDeductionPerMonth = Ok core.dependentDeductionPerMonth
    }
    member this.IsOfStd (std: Standard) =
        match this.personalDeductionPerMonth, this.dependentDeductionPerMonth with
        | Ok personalDeductionPerMonth, Ok dependentDeductionPerMonth ->
            let core =
                match std with
                | Std2025 -> Core.Setting.default2025
                | Std2026 -> Core.Setting.default2026
            personalDeductionPerMonth = core.personalDeductionPerMonth
            && dependentDeductionPerMonth = core.dependentDeductionPerMonth
        | _ ->
            false

type Model = {
    sources: IncomeSource list
    dependents: Dependent list
    setting: Setting
    result: Core.TaxResult option
} with
    member this.IsInputOk =
        this.sources
        |> List.forall (fun src ->
            seq {
                src.totalIncome
                src.insuranceDeduction
                src.otherDeduction
                src.withhold
            } |> Results.allOk
        )
        &&
            seq {
                this.setting.personalDeductionPerMonth
                this.setting.dependentDeductionPerMonth
            } |> Results.allOk

    member this.CoreProfile: Core.Profile = {
        sources =
            this.sources
            |> List.map (fun src ->
                {
                    totalIncome = src.totalIncome.Value
                    beginMonth = src.beginMonth
                    endMonth = src.endMonth
                    insuranceDeduction = src.insuranceDeduction.Value
                    otherDeduction = src.otherDeduction.Value
                    withhold = src.withhold.Value
                }
            )
        dependents =
            this.dependents
            |> List.map (fun dep ->
                {
                    beginMonth = dep.beginMonth
                    endMonth = dep.endMonth
                }
            )
    }

    member this.CoreSetting : Core.Setting = {
        personalDeductionPerMonth = this.setting.personalDeductionPerMonth.Value
        dependentDeductionPerMonth = this.setting.dependentDeductionPerMonth.Value
    }

type Msg =
    | AddIncomeSource
    | DeleteIncomeSource of index: int
    | ChangeCompanyName of index: int * name: string
    | ChangeTotalIncome of index: int * value: string
    | ChangeIncomeSourceBeginMonth of index: int * value: int
    | ChangeIncomeSourceEndMonth of index: int * value: int
    | ChangeInsuranceDeduction of index: int * value: string
    | ChangeOtherDeduction of index: int * value: string
    | ChangeWithhold of index: int * value: string

    | AddDependent
    | DeleteDependent of index: int
    | ChangeDependentName of index: int * name: string
    | ChangeDependentBeginMonth of index: int * value: int
    | ChangeDependentEndMonth of index: int * value: int
    | ChangePersonalDeductionPerMonth of value: string
    | ChangeDependentPerMonth of value: string

    | UseStandardSeting of Standard

    | Calculate
