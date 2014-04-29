object FrameFunctionParams2: TFrameFunctionParams2
  Left = 0
  Top = 0
  Width = 747
  Height = 615
  HorzScrollBar.Visible = False
  TabOrder = 0
  object pnlParams: TPanel
    Left = 0
    Top = 97
    Width = 747
    Height = 518
    Align = alClient
    TabOrder = 1
    object grpUnspecified: TGroupBox
      Left = 0
      Top = 0
      Width = 129
      Height = 93
      TabOrder = 0
      object pnlUnspecified: TPanel
        Left = 2
        Top = 15
        Width = 125
        Height = 58
        Align = alTop
        BevelOuter = bvNone
        Caption = '(Select a function type)'
        TabOrder = 0
      end
    end
    object grpPoint: TGroupBox
      Left = 288
      Top = 8
      Width = 129
      Height = 69
      Caption = 'Parameter '
      TabOrder = 1
      Visible = False
      object lblValuePoint: TLabel
        Left = 8
        Top = 20
        Width = 30
        Height = 13
        Caption = 'Value:'
      end
      object reValuePoint: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpGaussian: TGroupBox
      Left = 24
      Top = 104
      Width = 129
      Height = 133
      Caption = 'Parameters '
      TabOrder = 2
      Visible = False
      object lblMeanGaussian: TLabel
        Left = 8
        Top = 20
        Width = 30
        Height = 13
        Caption = 'Mean:'
      end
      object lblStdDevGaussian: TLabel
        Left = 8
        Top = 64
        Width = 46
        Height = 26
        Caption = 'Standard deviation:'
        WordWrap = True
      end
      object reMeanGaussian: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reStdDevGaussian: TREEdit
        Left = 8
        Top = 92
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpTriang: TGroupBox
      Left = 152
      Top = 252
      Width = 129
      Height = 157
      Caption = 'Parameters '
      TabOrder = 3
      Visible = False
      object lblMinTriang: TLabel
        Left = 8
        Top = 20
        Width = 44
        Height = 13
        Caption = 'Minimum:'
      end
      object lblModeTriang: TLabel
        Left = 8
        Top = 64
        Width = 30
        Height = 13
        Caption = 'Mode:'
      end
      object lblMaxTriang: TLabel
        Left = 8
        Top = 108
        Width = 47
        Height = 13
        Caption = 'Maximum:'
      end
      object reMinTriang: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reModeTriang: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reMaxTriang: TREEdit
        Left = 8
        Top = 124
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 2
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpGamma: TGroupBox
      Left = 152
      Top = 124
      Width = 129
      Height = 113
      Caption = 'Parameters '
      TabOrder = 5
      Visible = False
      object lblAlphaGamma: TLabel
        Left = 8
        Top = 20
        Width = 30
        Height = 13
        Caption = 'Alpha:'
      end
      object lblBetaGamma: TLabel
        Left = 8
        Top = 64
        Width = 25
        Height = 13
        Caption = 'Beta:'
        WordWrap = True
      end
      object reAlphaGamma: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reBetaGamma: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpExponential: TGroupBox
      Left = 144
      Top = 4
      Width = 129
      Height = 69
      Caption = 'Parameter '
      TabOrder = 6
      Visible = False
      object lblMeanExponential: TLabel
        Left = 8
        Top = 20
        Width = 30
        Height = 13
        Caption = 'Mean:'
      end
      object reMeanExponential: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpLogistic: TGroupBox
      Left = 288
      Top = 124
      Width = 129
      Height = 113
      Caption = 'Parameters '
      TabOrder = 7
      Visible = False
      object lblLocationLogistic: TLabel
        Left = 8
        Top = 20
        Width = 44
        Height = 13
        Caption = 'Location:'
      end
      object lblScaleLogistic: TLabel
        Left = 8
        Top = 64
        Width = 30
        Height = 13
        Caption = 'Scale:'
        WordWrap = True
      end
      object reLocationLogistic: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reScaleLogistic: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpPiecewise: TGroupBox
      Left = 432
      Top = 16
      Width = 129
      Height = 93
      Caption = 'Parameters '
      TabOrder = 8
      Visible = False
      object btnEditPiecewise: TButton
        Left = 28
        Top = 32
        Width = 73
        Height = 37
        Cancel = True
        Caption = 'View/Edit Points'
        TabOrder = 0
        WordWrap = True
        OnClick = btnEditPiecewiseClick
      end
    end
    object grpLoglogistic: TGroupBox
      Left = 16
      Top = 252
      Width = 129
      Height = 157
      Caption = 'Parameters '
      TabOrder = 9
      Visible = False
      object lblLocationLoglogistic: TLabel
        Left = 8
        Top = 20
        Width = 44
        Height = 13
        Caption = 'Location:'
      end
      object lblScaleLoglogistic: TLabel
        Left = 8
        Top = 64
        Width = 30
        Height = 13
        Caption = 'Scale:'
      end
      object lblShapeLoglogistic: TLabel
        Left = 8
        Top = 108
        Width = 34
        Height = 13
        Caption = 'Shape:'
      end
      object reLocationLoglogistic: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reScaleLoglogistic: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reShapeLoglogistic: TREEdit
        Left = 8
        Top = 124
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 2
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpUniform: TGroupBox
      Left = 424
      Top = 124
      Width = 129
      Height = 113
      Caption = 'Parameters '
      TabOrder = 10
      Visible = False
      object lblMinUniform: TLabel
        Left = 8
        Top = 20
        Width = 44
        Height = 13
        Caption = 'Minimum:'
      end
      object lblMaxUniform: TLabel
        Left = 8
        Top = 64
        Width = 47
        Height = 13
        Caption = 'Maximum:'
      end
      object reMinUniform: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reMaxUniform: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpLognormal2: TGroupBox
      Left = 560
      Top = 124
      Width = 129
      Height = 113
      Caption = 'Parameters '
      TabOrder = 11
      Visible = False
      object lblZetaLognormal2: TLabel
        Left = 8
        Top = 20
        Width = 25
        Height = 13
        Caption = 'Zeta:'
      end
      object lblSigmaLognormal2: TLabel
        Left = 8
        Top = 64
        Width = 34
        Height = 13
        Caption = 'Sigma'#39':'
      end
      object reZetaLognormal2: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reSigmaLognormal2: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object grpBeta: TGroupBox
      Left = 288
      Top = 252
      Width = 129
      Height = 201
      Caption = 'Parameters '
      TabOrder = 4
      Visible = False
      object lblAlpha1Beta: TLabel
        Left = 8
        Top = 20
        Width = 36
        Height = 13
        Caption = 'Alpha1:'
      end
      object lblAlpha2Beta: TLabel
        Left = 8
        Top = 64
        Width = 36
        Height = 13
        Caption = 'Alpha2:'
      end
      object lblMinBeta: TLabel
        Left = 8
        Top = 108
        Width = 44
        Height = 13
        Caption = 'Minimum:'
      end
      object lblMaxBeta: TLabel
        Left = 8
        Top = 152
        Width = 47
        Height = 13
        Caption = 'Maximum:'
      end
      object reAlpha1Beta: TREEdit
        Left = 8
        Top = 36
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 0
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reAlpha2Beta: TREEdit
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 1
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reMinBeta: TREEdit
        Left = 8
        Top = 124
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 2
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
      object reMaxBeta: TREEdit
        Left = 8
        Top = 168
        Width = 65
        Height = 21
        EditAlign = eaLeft
        TabOrder = 3
        OnExit = reValueChanged
        OnKeyDown = rleKeyDown
      end
    end
    object btnConvert: TButton
      Left = 72
      Top = 216
      Width = 89
      Height = 41
      Caption = 'Convert to piecewise...'
      TabOrder = 12
      WordWrap = True
    end
    inline fraAcceptCancel: TFrameAcceptCancel
      Left = 136
      Top = 80
      Width = 44
      Height = 24
      HorzScrollBar.Visible = False
      VertScrollBar.Visible = False
      TabOrder = 13
      Visible = False
      inherited btnAccept: TBitBtn
        Enabled = True
        OnClick = fraAcceptCancelbtnAcceptClick
      end
      inherited btnCancel: TBitBtn
        Enabled = True
        OnClick = fraAcceptCancelbtnCancelClick
      end
    end
    inline fraPointEditorGrid: TFramePointEditorGrid
      Left = 450
      Top = 272
      Width = 217
      Height = 161
      TabOrder = 14
      inherited pnlButtonContainer: TPanel
        Width = 217
        inherited pnlButtons: TPanel
          inherited btnOK: TBitBtn
            OnClick = fraPointEditorGridbtnOKClick
          end
          inherited btnCancel: TBitBtn
            OnClick = fraPointEditorGridbtnCancelClick
          end
        end
      end
      inherited stgPoints: TStringGrid
        Width = 217
        Height = 120
        OnEnter = fraPointEditorGridstgPointsEnter
        OnKeyDown = fraPointEditorGridstgPointsKeyDown
      end
    end
  end
  object pnlCbos: TPanel
    Left = 0
    Top = 0
    Width = 747
    Height = 97
    Align = alTop
    TabOrder = 0
    object lblPdfType: TLabel
      Left = 8
      Top = 8
      Width = 67
      Height = 13
      Caption = 'Function type:'
    end
    object lblXAxis: TLabel
      Left = 8
      Top = 48
      Width = 56
      Height = 13
      Caption = 'X axis units:'
    end
    object lblYAxis: TLabel
      Left = 152
      Top = 48
      Width = 56
      Height = 13
      Caption = 'Y axis units:'
      Visible = False
    end
    object cboPdfType: TComboBox
      Left = 8
      Top = 24
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cboPdfTypeChange
    end
    object cboXAxis: TComboBox
      Left = 8
      Top = 64
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object cboYAxis: TComboBox
      Left = 152
      Top = 64
      Width = 113
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
      Visible = False
    end
  end
end
