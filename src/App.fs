module App

open System
open Fastoch.Feliz
open Fastoch.Elmish

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

type Setting = {
    personalDeductionPerMonth: MoneyInput
    dependentDeductionPerMonth: MoneyInput
} with
    static member defaultValue = {
        personalDeductionPerMonth = Ok 11_000_000
        dependentDeductionPerMonth = Ok 4_400_000
    }

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
    | ChangeDeductionSettingPersonalPerMonth of value: string
    | ChangeDeductionSettingDependentPerMonth of value: string
    | Calculate

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

    | ChangeDeductionSettingPersonalPerMonth value ->
        { model with
            setting.personalDeductionPerMonth = MoneyInput.parse value
        }

    | ChangeDeductionSettingDependentPerMonth value ->
        { model with
            setting.dependentDeductionPerMonth = MoneyInput.parse value
        }

    | Calculate ->
        if not model.IsInputOk then
            { model with result = None }
        else
            let result = Core.calculate model.CoreSetting model.CoreProfile
            { model with result = Some result }

let view dispatch (model: Model) =
    let renderMonthSelection (label: string) (selected: int) (onChange: string -> unit) =
        Html.p [
            Html.label label
            Html.select [
                prop.onChange onChange
                prop.children [
                    for month = 1 to 12 do
                        let sel = selected = month
                        Html.option [ prop.value month; prop.text month; prop.selected sel ]
                ]
            ]
        ]

    let renderMoneyInput (mi: MoneyInput) (onChange: string -> unit) =
        let value, styles =
            match mi with
            | Ok value -> formatNumber value, []
            | Error value -> value, [ style.borderColor.red; style.color.red ]
        Html.input [
            prop.type'.text
            prop.value value
            prop.onChange onChange
            prop.style styles
        ]

    let renderIncomeSource (canDelete: bool) (index: int) (src: IncomeSource) =
        Html.details [
            let companyName =
                if String.IsNullOrWhiteSpace src.companyName then $"Công ty {index + 1}" else src.companyName
            let totalIncomeSuffix =
                match src.totalIncome with Ok n -> " (" + formatNumber n + ")" | Error _ -> ""
            Html.summary $"{index + 1}. {companyName}{totalIncomeSuffix}"
            Html.p [
                Html.label "Tên công ty"
                Html.input [
                    prop.type'.text
                    prop.value src.companyName
                    prop.onChange (fun name -> dispatch (ChangeCompanyName (index, name)))
                ]
            ]
            Html.p [
                Html.label "Tổng thu nhập"
                renderMoneyInput src.totalIncome (fun value -> dispatch (ChangeTotalIncome (index, value)))
            ]
            renderMonthSelection "Tháng bắt đầu" src.beginMonth
                (fun value -> dispatch (ChangeIncomeSourceBeginMonth (index, int value)))
            renderMonthSelection "Tháng kết thúc" src.endMonth
                (fun value -> dispatch (ChangeIncomeSourceEndMonth (index, int value)))
            Html.p [
                Html.label "Bảo hiểm được trừ"
                renderMoneyInput src.insuranceDeduction
                    (fun value -> dispatch (ChangeInsuranceDeduction (index, value)))
            ]
            Html.p [
                Html.label "Được trừ khác (từ thiện...)"
                renderMoneyInput src.otherDeduction (fun value -> dispatch (ChangeOtherDeduction (index, value)))
            ]
            Html.p [
                Html.label "Số thuế đã khấu trừ"
                renderMoneyInput src.withhold (fun value -> dispatch (ChangeWithhold (index, value)))
            ]
            Html.button [
                prop.text "❌ Xóa"
                prop.onClick (fun _ -> dispatch (DeleteIncomeSource index))
                prop.disabled (not canDelete)
            ]
        ]

    let renderDependent (index: int) (dep: Dependent) =
        Html.details [
            let name = if String.IsNullOrWhiteSpace dep.name then $"NPT {index + 1}" else dep.name
            Html.summary $"{index + 1}. {name} ({dep.beginMonth}-{dep.endMonth})"
            Html.p [
                Html.label "Tên"
                Html.input [
                    prop.type'.text
                    prop.value dep.name
                    prop.onChange (fun name -> dispatch (ChangeDependentName (index, name)))
                ]
            ]
            renderMonthSelection "Tháng bắt đầu" dep.beginMonth
                (fun value -> dispatch (ChangeDependentBeginMonth (index, int value)))
            renderMonthSelection "Tháng kết thúc" dep.endMonth
                (fun value -> dispatch (ChangeDependentEndMonth (index, int value)))
            Html.button [ prop.text "❌ Xóa"; prop.onClick (fun _ -> dispatch (DeleteDependent index)) ]
        ]

    Html.main [
        Html.h3 "Nguồn thu nhập"
        yield! model.sources |> List.mapi (renderIncomeSource (model.sources.Length > 1))
        Html.button [ prop.text "＋ Thêm"; prop.onClick (fun _ -> dispatch AddIncomeSource) ]

        Html.h3 "Người phụ thuộc"
        if model.dependents.IsEmpty then
            Html.p [ prop.text "Không có người phụ thuộc nào"; prop.style [ style.fontStyle.italic ] ]
        else
            yield! model.dependents |> List.mapi renderDependent
        Html.button [ prop.text "＋ Thêm"; prop.onClick (fun _ -> dispatch AddDependent) ]

        Html.h3 "Thông số"
        Html.p [
            Html.label "Giảm trừ bản thân mỗi tháng"
            renderMoneyInput model.setting.personalDeductionPerMonth
                (dispatch << ChangeDeductionSettingPersonalPerMonth)
        ]
        Html.p [
            Html.label "Giảm trừ người phụ thuộc mỗi tháng"
            renderMoneyInput model.setting.dependentDeductionPerMonth
                (dispatch << ChangeDeductionSettingDependentPerMonth)
        ]

        Html.hr []

        Html.button [
            prop.text "🚀 Tính thuế"
            prop.disabled (not model.IsInputOk)
            prop.onClick (fun _ -> dispatch Calculate)
        ]

        match model.result with
        | None -> Html.none
        | Some res ->
            let thRight (text: string) = Html.th [
                prop.style [ style.textAlign.right ]
                prop.text text
            ]
            let tdRight (text: string) = Html.td [
                prop.style [ style.textAlign.right ]
                prop.text text
            ]
            Html.table [
                Html.thead [
                    Html.tr [ Html.th "Tổng thu nhập"; thRight (formatNumber res.totalIncome) ]
                ]
                Html.tbody [
                    Html.tr [ Html.td "Giảm trừ bản thân"; tdRight (formatNumber res.personalDeduction) ]
                    Html.tr [ Html.td "Giảm trừ người phụ thuộc"; tdRight (formatNumber res.dependentDeduction) ]
                    Html.tr [ Html.td "Bảo hiểm được trừ"; tdRight (formatNumber res.insuranceDeduction) ]
                    Html.tr [ Html.td "Được trừ khác"; tdRight (formatNumber res.otherDeduction) ]
                    Html.tr [ Html.td "Thu nhập tính thuế"; tdRight (formatNumber res.taxedIncome) ]
                    Html.tr [ Html.td "Số thuế phải đóng"; tdRight (res.tax |> roundInt64 |> formatNumber) ]
                    Html.tr [ Html.td "Số thuế đã khấu trừ"; tdRight (formatNumber res.withhold) ]
                    let pay = res.pay |> roundInt64
                    if pay >= 0 then
                        Html.tr [ Html.td "❌ Số thuế còn thiếu"; tdRight (formatNumber pay) ]
                    else
                        Html.tr [ Html.td "✅ Số thuế được hoàn"; tdRight (formatNumber -pay) ]
                ]
            ]
    ]

Program.mkSimple init update view
|> Program.withFastoch "app"
#if DEBUG
|> Program.withTrace (fun msg model _ -> printfn "msg: %A\nmodel: %A" msg model)
#endif
|> Program.run
