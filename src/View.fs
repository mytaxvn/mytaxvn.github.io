module View

open System
open Fastoch.Feliz
open Types

let private renderMonthSelection (label: string) (selected: int) (onChange: int -> unit) =
    Html.p [
        Html.label label
        Html.select [
            prop.onChange (fun (value: string) -> value |> int |> onChange)
            prop.children [
                for month = 1 to 12 do
                    let sel = selected = month
                    Html.option [ prop.value month; prop.text month; prop.selected sel ]
            ]
        ]
    ]

let private renderMoneyInput (mi: MoneyInput) (onChange: string -> unit) =
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

let private renderIncomeSource dispatch (canDelete: bool) (index: int) (src: IncomeSource) =
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
            (fun value -> dispatch (ChangeIncomeSourceBeginMonth (index, value)))
        renderMonthSelection "Tháng kết thúc" src.endMonth
            (fun value -> dispatch (ChangeIncomeSourceEndMonth (index, value)))
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

let private renderDependent dispatch (index: int) (dep: Dependent) =
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
            (fun value -> dispatch (ChangeDependentBeginMonth (index, value)))
        renderMonthSelection "Tháng kết thúc" dep.endMonth
            (fun value -> dispatch (ChangeDependentEndMonth (index, value)))
        Html.button [ prop.text "❌ Xóa"; prop.onClick (fun _ -> dispatch (DeleteDependent index)) ]
    ]

let private renderResult (res: Core.TaxResult) =
    let tdRight (text: string) = Html.td [
        prop.style [ style.textAlign.right ]
        prop.text text
    ]
    Html.table [
        Html.tbody [
            Html.tr [
                Html.td "Tổng thu nhập"
                tdRight (formatNumber res.totalIncome)
                Html.td "[1]"
            ]
            Html.tr [
                Html.td "Giảm trừ bản thân"
                tdRight (formatNumber res.personalDeduction)
                Html.td "[2]"
            ]
            Html.tr [
                Html.td "Giảm trừ người phụ thuộc"
                tdRight (formatNumber res.dependentDeduction)
                Html.td "[3]"
            ]
            Html.tr [
                Html.td "Bảo hiểm được trừ"
                tdRight (formatNumber res.insuranceDeduction)
                Html.td "[4]"
            ]
            Html.tr [
                Html.td "Được trừ khác"
                tdRight (formatNumber res.otherDeduction)
                Html.td "[5]"
            ]
            Html.tr [
                Html.td "Thu nhập tính thuế"
                tdRight (formatNumber res.taxedIncome)
                Html.td "[6] = [1]-[2]-[3]-[4]-[5]"
            ]
            Html.tr [
                Html.td "Số thuế phải đóng"
                tdRight (res.tax |> roundInt64 |> formatNumber)
                Html.td [
                    Html.span "[7] ="
                    Html.a [
                        prop.href "https://thuvienphapluat.vn/chinh-sach-phap-luat-moi/vn/ho-tro-phap-luat/chinh-sach-moi/82461/thue-thu-nhap-ca-nhan-2025-muc-dong-va-cach-tinh-thue-tu-tien-luong-tien-cong"
                        prop.text " Tính"
                    ]
                    Html.span " trên [6]"
                ]
            ]
            Html.tr [
                Html.td "Số thuế đã khấu trừ"
                tdRight (formatNumber res.withhold)
                Html.td "[8]"
            ]
            let pay = res.pay |> roundInt64
            if pay >= 0 then
                Html.tr [
                    Html.td "❌ Số thuế còn thiếu"; tdRight (formatNumber pay); Html.td "[9] = [7]-[8]"
                ]
            else
                Html.tr [
                    Html.td "✅ Số thuế được hoàn"; tdRight (formatNumber -pay); Html.td "[9] = [8]-[7]"
                ]
        ]
    ]

let render dispatch (model: Model) =
    Html.main [
        Html.h3 "Nguồn thu nhập"
        yield! model.sources |> List.mapi (renderIncomeSource dispatch (model.sources.Length > 1))
        Html.button [ prop.text "＋ Thêm"; prop.onClick (fun _ -> dispatch AddIncomeSource) ]

        Html.h3 "Người phụ thuộc"
        if model.dependents.IsEmpty then
            Html.p [ prop.text "Không có người phụ thuộc nào"; prop.style [ style.fontStyle.italic ] ]
        else
            yield! model.dependents |> List.mapi (renderDependent dispatch)
        Html.button [ prop.text "＋ Thêm"; prop.onClick (fun _ -> dispatch AddDependent) ]

        Html.h3 "Thông số"
        Html.p [
            Html.label "Giảm trừ bản thân mỗi tháng"
            renderMoneyInput model.setting.personalDeductionPerMonth (dispatch << ChangePersonalDeductionPerMonth)
        ]
        Html.p [
            Html.label "Giảm trừ người phụ thuộc mỗi tháng"
            renderMoneyInput model.setting.dependentDeductionPerMonth (dispatch << ChangeDependentPerMonth)
        ]

        Html.hr []
        Html.button [
            prop.text "🚀 Tính thuế"
            prop.disabled (not model.IsInputOk)
            prop.onClick (fun _ -> dispatch Calculate)
        ]

        match model.result with
        | None -> Html.none
        | Some res -> renderResult res
    ]
