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

let private renderMoneyInput (input: MoneyInput) (onChange: string -> unit) =
    let value, styles =
        match input with
        | Ok n -> formatNumber n, []
        | Error str -> str, [ style.color.red ]
    Html.input [
        prop.type'.text
        prop.value value
        prop.onChange onChange
        prop.style styles
    ]

let private renderIncomeSource dispatch (canDelete: bool) (index: int) (src: IncomeSource) =
    Html.details [
        let companyName =
            if String.IsNullOrWhiteSpace src.companyName
            then $"Công ty {index + 1}"
            else src.companyName
        let totalIncomeSuffix =
            match src.totalIncome with
            | Ok n -> $" ({formatNumber n})"
            | Error _ -> ""
        Html.summary $"{index + 1}. {companyName}{totalIncomeSuffix}"
        Html.p [
            Html.label "Tên công ty"
            Html.input [
                prop.type'.text
                prop.value src.companyName
                prop.onChange (tup index >> ChangeCompanyName >> dispatch)
            ]
        ]
        Html.p [
            Html.label "Tổng thu nhập"
            renderMoneyInput src.totalIncome (tup index >> ChangeTotalIncome >> dispatch)
        ]
        renderMonthSelection "Tháng bắt đầu" src.beginMonth (tup index >> ChangeIncomeSourceBeginMonth >> dispatch)
        renderMonthSelection "Tháng kết thúc" src.endMonth (tup index >> ChangeIncomeSourceEndMonth >> dispatch)
        Html.p [
            Html.label "Bảo hiểm được trừ"
            renderMoneyInput src.insuranceDeduction (tup index >> ChangeInsuranceDeduction >> dispatch)
        ]
        Html.p [
            Html.label "Được trừ khác (từ thiện...)"
            renderMoneyInput src.otherDeduction (tup index >> ChangeOtherDeduction >> dispatch)
        ]
        Html.p [
            Html.label "Số thuế đã khấu trừ"
            renderMoneyInput src.withhold (tup index >> ChangeWithhold >> dispatch)
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
                prop.onChange (tup index >> ChangeDependentName >> dispatch)
            ]
        ]
        renderMonthSelection "Tháng bắt đầu" dep.beginMonth (tup index >> ChangeDependentBeginMonth >> dispatch)
        renderMonthSelection "Tháng kết thúc" dep.endMonth (tup index >> ChangeDependentEndMonth >> dispatch)
        Html.button [ prop.text "❌ Xóa"; prop.onClick (fun _ -> dispatch (DeleteDependent index)) ]
    ]

let private renderSetting dispatch (setting: Setting) (currentStd: Std option) = [
    let button std styles =
        let label = match std with Std2025 -> "2025" | Std2026 -> "2026"
        let selected = currentStd = Some std
        let prefix = if selected then "✓ " else ""
        Html.button [
            prop.text (prefix + label)
            prop.onClick (fun _ -> dispatch (UseStandardSeting std))
            prop.disabled selected
            prop.style styles
        ]
    Html.p [
        button Std2025 []
        button Std2026 [ style.marginLeft (length.px 10) ]
    ]
    Html.p [
        Html.label "Giảm trừ bản thân mỗi tháng"
        renderMoneyInput setting.personalDeductionPerMonth (ChangePersonalDeductionPerMonth >> dispatch)
    ]
    Html.p [
        Html.label "Giảm trừ người phụ thuộc mỗi tháng"
        renderMoneyInput setting.dependentDeductionPerMonth (ChangeDependentDeductionPerMonth >> dispatch)
    ]
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
                        prop.target.blank
                    ]
                    Html.span " trên [6]"
                ]
            ]
            Html.tr [
                Html.td "Số thuế đã khấu trừ"
                tdRight (formatNumber res.withhold)
                Html.td "[8]"
            ]
            Html.tr [
                let pay = res.pay |> roundInt64
                if pay >= 0 then
                    Html.td "❌ Số thuế còn thiếu"; tdRight (formatNumber pay); Html.td "[9] = [7]-[8]"
                else
                    Html.td "✅ Số thuế được hoàn"; tdRight (formatNumber -pay); Html.td "[9] = [8]-[7]"
            ]
        ]
    ]

let render dispatch (model: Model) =
    Html.main [
        Html.article [
            Html.h3 "Nguồn thu nhập"
            yield! model.sources |> List.mapi (renderIncomeSource dispatch (model.sources.Length > 1))
            Html.button [ prop.text "＋ Thêm"; prop.onClick (fun _ -> dispatch AddIncomeSource) ]
        ]

        Html.article [
            Html.h3 "Người phụ thuộc"
            if model.dependents.IsEmpty then
                Html.p [ prop.text "Không có người phụ thuộc nào"; prop.style [ style.fontStyle.italic ] ]
            else
                yield! model.dependents |> List.mapi (renderDependent dispatch)
            Html.button [ prop.text "＋ Thêm"; prop.onClick (fun _ -> dispatch AddDependent) ]
        ]

        Html.article [
            Html.h3 "Thông số"
            yield! renderSetting dispatch model.setting model.currentStd
        ]

        Html.article [
            Html.h3 "🚀 Kết quả"
            match model.result with
            | None ->
                Html.p [
                    prop.text "Có lỗi trong phần nhập liệu phía trên. Vui lòng nhập lại."
                    prop.style [ style.color.red; style.fontStyle.italic ]
                ]
            | Some res -> renderResult res
        ]
    ]
