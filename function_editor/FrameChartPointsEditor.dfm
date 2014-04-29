inherited FrameChartPointsEditor: TFrameChartPointsEditor
  Width = 536
  Height = 474
  object pnlBottom: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 474
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblErrorMessage: TLabel
      Left = 128
      Top = 280
      Width = 116
      Height = 16
      Alignment = taCenter
      Caption = 'lblErrorMessage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object pnlChart: TPanel
    Left = 136
    Top = 8
    Width = 305
    Height = 249
    TabOrder = 1
    object pnlCumulativeContainer: TPanel
      Left = 1
      Top = 1
      Width = 303
      Height = 25
      Align = alTop
      TabOrder = 0
      object pnlCumulative: TPanel
        Left = 1
        Top = 1
        Width = 301
        Height = 23
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object cbxShowCumProb: TCheckBox
          Left = 6
          Top = 4
          Width = 291
          Height = 17
          Caption = 'Show cumulative probability'
          TabOrder = 0
          OnClick = cbxShowCumProbClick
        end
      end
    end
    object chartMain: TChart
      Left = 1
      Top = 26
      Width = 303
      Height = 222
      AllowPanning = pmNone
      AllowZoom = False
      BackWall.Brush.Color = clWhite
      BackWall.Brush.Style = bsClear
      Title.Text.Strings = (
        'Probability Density Function')
      Title.Visible = False
      BottomAxis.Title.Caption = 'x units go here:  Title property- > Axis -> Title'
      LeftAxis.Title.Caption = 'y units go here:  Title property- > Axis -> Title'
      Legend.Visible = False
      View3D = False
      Align = alClient
      PopupMenu = PopupMenu
      TabOrder = 1
      OnMouseDown = chartMainMouseDown
      OnMouseUp = chartMainMouseUp
      object seriesPDF: TAreaSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clBlue
        Title = 'seriesPDF'
        DrawArea = True
        Pointer.HorizSize = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.VertSize = 2
        Pointer.Visible = True
        XValues.DateTime = False
        XValues.Name = 'X'
        XValues.Multiplier = 1.000000000000000000
        XValues.Order = loAscending
        YValues.DateTime = False
        YValues.Name = 'Y'
        YValues.Multiplier = 1.000000000000000000
        YValues.Order = loNone
      end
      object seriesCumulative: TFastLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clRed
        Title = 'seriesCumulative'
        LinePen.Color = clRed
        XValues.DateTime = False
        XValues.Name = 'X'
        XValues.Multiplier = 1.000000000000000000
        XValues.Order = loAscending
        YValues.DateTime = False
        YValues.Name = 'Y'
        YValues.Multiplier = 1.000000000000000000
        YValues.Order = loNone
      end
      object SeriesREL: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clGreen
        Pointer.HorizSize = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.VertSize = 2
        Pointer.Visible = True
        XValues.DateTime = False
        XValues.Name = 'X'
        XValues.Multiplier = 1.000000000000000000
        XValues.Order = loAscending
        YValues.DateTime = False
        YValues.Name = 'Y'
        YValues.Multiplier = 1.000000000000000000
        YValues.Order = loNone
      end
      object serYAxisMarker: TLineSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clYellow
        Title = 'serZeroMarker'
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.DateTime = False
        XValues.Name = 'X'
        XValues.Multiplier = 1.000000000000000000
        XValues.Order = loAscending
        YValues.DateTime = False
        YValues.Name = 'Y'
        YValues.Multiplier = 1.000000000000000000
        YValues.Order = loNone
      end
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 16
    Top = 9
    object mnuAddPoint: TMenuItem
      Caption = 'Add point'
      OnClick = mnuAddPointClick
    end
    object mnuRemovePoint: TMenuItem
      Caption = 'Remove point'
      OnClick = mnuRemovePointClick
    end
  end
end
