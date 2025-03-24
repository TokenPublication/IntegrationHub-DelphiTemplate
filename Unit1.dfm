object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'POS Communication Demo'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object btnSendBasket: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Send Basket'
    TabOrder = 0
    OnClick = btnSendBasketClick
  end
  object btnSendPayment: TButton
    Left = 89
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Send Payment'
    TabOrder = 1
    OnClick = btnSendPaymentClick
  end
  object btnGetFiscalInfo: TButton
    Left = 184
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Get Fiscal Info'
    TabOrder = 2
    OnClick = btnGetFiscalInfoClick
  end
  object memLog: TMemo
    Left = 8
    Top = 39
    Width = 612
    Height = 395
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
