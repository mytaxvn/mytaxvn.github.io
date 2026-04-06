module State

open System
open Types

module MoneyInput =
    let create (x: int64) : MoneyInput = Ok x

    let zero = create 0L

    let parse (str: string) : MoneyInput =
        if String.IsNullOrWhiteSpace str then
            zero
        else
            match tryParseInt64 str with
            | Some n -> Ok n
            | None -> Error str

module IncomeSource =
    let defaultValue = {
        companyName = ""
        totalIncome = MoneyInput.zero
        beginMonth = 1
        endMonth = 12
        insuranceDeduction = MoneyInput.zero
        otherDeduction = MoneyInput.zero
        withhold = MoneyInput.zero
    }

module Dependent =
    let defaultValue = {
        name = ""
        beginMonth = 1
        endMonth = 12
    }

module Setting =
    let private fromCore (core: Core.Setting) = {
        personalDeductionPerMonth = MoneyInput.create core.personalDeductionPerMonth
        dependentDeductionPerMonth = MoneyInput.create core.dependentDeductionPerMonth
    }

    let default2025 = fromCore Core.Setting.default2025
    let default2026 = fromCore Core.Setting.default2026

module Model =
    let private isInputOk (model: Model) =
        model.sources
        |> List.forall (fun src ->
            seq {
                src.totalIncome
                src.insuranceDeduction
                src.otherDeduction
                src.withhold
            }
            |> Results.allOk
        )
        &&
        seq {
            model.setting.personalDeductionPerMonth
            model.setting.dependentDeductionPerMonth
        }
        |> Results.allOk

    let private extractCoreProfile (model: Model) : Core.Profile =
        {
            sources =
                model.sources
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
                model.dependents
                |> List.map (fun dep ->
                    {
                        beginMonth = dep.beginMonth
                        endMonth = dep.endMonth
                    }
                )
        }

    let private extractCoreSetting (model: Model) : Core.Setting = {
        personalDeductionPerMonth = model.setting.personalDeductionPerMonth.Value
        dependentDeductionPerMonth = model.setting.dependentDeductionPerMonth.Value
    }

    let calculate (model: Model) =
        { model with
            currentStd =
                if model.setting = Setting.default2025 then Some Std2025
                elif model.setting = Setting.default2026 then Some Std2026
                else None
        }
        |> fun model ->
            if model |> isInputOk then
                let result = Core.calculate (model |> extractCoreSetting) (model |> extractCoreProfile)
                { model with result = Some result }
            else
                { model with result = None }

let init () =
    {
        sources = [ IncomeSource.defaultValue ]
        dependents = []
        setting = Setting.default2025
        currentStd = Some Std2025
        result = None
    }
    |> Model.calculate

let private update' (msg: Msg) (model: Model) =
    let changeIncomeSourceMoneyInput index value change =
        if index < 0 || index >= model.sources.Length then model
        else
            { model with
                sources = model.sources |> List.changeAt index (change (MoneyInput.parse value))
            }

    match msg with
    | AddIncomeSource ->
        { model with sources = model.sources @ [ IncomeSource.defaultValue ] }

    | DeleteIncomeSource index ->
        if index < 0 || index >= model.sources.Length then model
        else
            { model with sources = model.sources |> List.removeAt index }

    | ChangeCompanyName (index, name) ->
        if index < 0 || index >= model.sources.Length then model
        else
            { model with
                sources =
                    model.sources
                    |> List.changeAt index (fun src -> { src with companyName = name })
            }

    | ChangeTotalIncome (index, value) ->
        changeIncomeSourceMoneyInput index value (fun input src -> { src with totalIncome = input } )

    | ChangeIncomeSourceBeginMonth (index, value)
    | ChangeIncomeSourceEndMonth (index, value) ->
        if index < 0 || index >= model.sources.Length || value < 1 || value > 12 then model
        else
            { model with
                sources =
                    model.sources
                    |> List.changeAt index (fun src ->
                        match msg with
                        | ChangeIncomeSourceBeginMonth _ ->
                            { src with beginMonth = value; endMonth = max value src.endMonth  }
                        | _ ->
                            { src with endMonth = value; beginMonth = min value src.beginMonth  }
                    )
            }

    | ChangeInsuranceDeduction(index, value) ->
        changeIncomeSourceMoneyInput index value (fun input src -> { src with insuranceDeduction = input } )

    | ChangeOtherDeduction(index, value) ->
        changeIncomeSourceMoneyInput index value (fun input src -> { src with otherDeduction = input } )

    | ChangeWithhold(index, value) ->
        changeIncomeSourceMoneyInput index value (fun input src -> { src with withhold = input } )

    | AddDependent ->
        { model with dependents = model.dependents @ [ Dependent.defaultValue ] }

    | DeleteDependent index ->
        if index < 0 || index >= model.dependents.Length then model
        else { model with dependents = model.dependents |> List.removeAt index }

    | ChangeDependentName (index, name) ->
        if index < 0 || index >= model.dependents.Length then model
        else
            { model with
                dependents =
                    model.dependents
                    |> List.changeAt index (fun dep -> { dep with name = name })
            }

    | ChangeDependentBeginMonth (index, value)
    | ChangeDependentEndMonth (index, value) ->
        if index < 0 || index >= model.dependents.Length || value < 1 || value > 12 then model
        else
            { model with
                dependents =
                    model.dependents
                    |> List.changeAt index (fun src ->
                        match msg with
                        | ChangeDependentBeginMonth _ ->
                            { src with beginMonth = value; endMonth = max value src.endMonth  }
                        | _ ->
                            { src with endMonth = value; beginMonth = min value src.beginMonth  }
                    )
            }

    | ChangePersonalDeductionPerMonth value ->
        { model with
            setting.personalDeductionPerMonth = MoneyInput.parse value }

    | ChangeDependentDeductionPerMonth value ->
        { model with
            setting.dependentDeductionPerMonth = MoneyInput.parse value }

    | UseStandardSeting std ->
        match std with
        | Std2025 -> { model with setting = Setting.default2025 }
        | Std2026 -> { model with setting = Setting.default2026 }

let update msg model = update' msg model |> Model.calculate
