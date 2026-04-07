module Types

type MoneyInput = Result<int64, string>

type IncomeSource = {
    companyName: string
    totalIncome: MoneyInput
    beginMonth: int
    endMonth: int
    insuranceDeduction: MoneyInput
    otherDeduction: MoneyInput
    withhold: MoneyInput
}

type Dependent = {
    name: string
    beginMonth: int
    endMonth: int
}

type Setting = {
    personalDeductionPerMonth: MoneyInput
    dependentDeductionPerMonth: MoneyInput
}

type Std =
    | Std2025
    | Std2026

type Model = {
    sources: IncomeSource list
    dependents: Dependent list
    setting: Setting
    selectedStd: Std option
    result: Core.TaxResult option
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
    | ChangeDependentDeductionPerMonth of value: string
    | UseStandardSeting of Std
