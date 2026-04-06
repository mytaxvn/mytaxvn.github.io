module State

open Types

let init () : Model = {
    sources = [ IncomeSource.defaultValue ]
    dependents = []
    setting = Setting.defaultValue
    result = None
}

let update (msg: Msg) (model: Model) =
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
        else { model with sources = model.sources |> List.removeAt index }

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
        { model with dependents = model.dependents @ [ Dependent.defaultvalue ] }

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
            setting.personalDeductionPerMonth = MoneyInput.parse value
        }

    | ChangeDependentPerMonth value ->
        { model with
            setting.dependentDeductionPerMonth = MoneyInput.parse value
        }

    | Calculate ->
        if not model.IsInputOk then
            { model with result = None }
        else
            let result = Core.calculate model.CoreSetting model.CoreProfile
            { model with result = Some result }
