unit FrameFunctionParams2;

(*
FrameFunctionParams2.pas/dfm
----------------------------
Begin: 2005/11/10
Last revision: $Date: 2008/11/25 22:05:57 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    Grids,
    ExtCtrls,

    REEdit,

    QStringMaps,
    FunctionPointers,

    Points,
    ChartFunction,
    ProbDensityFunctions,
    RelFunction,

    //FrameFunctionParamsBase,
    FrameAcceptCancel,
    FramePointEditorGrid
  ;

  type RPdfRecord = record
      min: double;
      max: double;
      mode: double;
      mean: double;
      stddev: double;
      alpha: double;
      alpha2: double;
      beta: double;
      location: double;
      scale: double;
      shape: double;

      pdfType: TPdfType;
      pdfUnits: TChartUnitType;
      name: string;
    end
  ;


  type TFrameFunctionParams2 = class( TFrame )
     pnlParams: TPanel;
      grpUnspecified: TGroupBox;
      grpPoint: TGroupBox;
      lblValuePoint: TLabel;
      reValuePoint: TREEdit;
      grpGaussian: TGroupBox;
      lblMeanGaussian: TLabel;
      lblStdDevGaussian: TLabel;
      reMeanGaussian: TREEdit;
      reStdDevGaussian: TREEdit;
      grpTriang: TGroupBox;
      lblMinTriang: TLabel;
      lblModeTriang: TLabel;
      lblMaxTriang: TLabel;
      reMinTriang: TREEdit;
      reModeTriang: TREEdit;
      reMaxTriang: TREEdit;
      grpBeta: TGroupBox;
      lblAlpha1Beta: TLabel;
      lblAlpha2Beta: TLabel;
      lblMinBeta: TLabel;
      lblMaxBeta: TLabel;
      reAlpha1Beta: TREEdit;
      reAlpha2Beta: TREEdit;
      reMinBeta: TREEdit;
      reMaxBeta: TREEdit;
      grpGamma: TGroupBox;
      lblAlphaGamma: TLabel;
      lblBetaGamma: TLabel;
      reAlphaGamma: TREEdit;
      reBetaGamma: TREEdit;
      grpExponential: TGroupBox;
      lblMeanExponential: TLabel;
      reMeanExponential: TREEdit;
      grpLogistic: TGroupBox;
      lblLocationLogistic: TLabel;
      lblScaleLogistic: TLabel;
      reLocationLogistic: TREEdit;
      reScaleLogistic: TREEdit;
      grpPiecewise: TGroupBox;
      btnEditPiecewise: TButton;
      grpLoglogistic: TGroupBox;
      lblLocationLoglogistic: TLabel;
      lblScaleLoglogistic: TLabel;
      lblShapeLoglogistic: TLabel;
      reLocationLoglogistic: TREEdit;
      reScaleLoglogistic: TREEdit;
      reShapeLoglogistic: TREEdit;
      grpUniform: TGroupBox;
      lblMinUniform: TLabel;
      lblMaxUniform: TLabel;
      reMinUniform: TREEdit;
      reMaxUniform: TREEdit;
      grpLognormal2: TGroupBox;
      lblZetaLognormal2: TLabel;
      lblSigmaLognormal2: TLabel;
      reZetaLognormal2: TREEdit;
      reSigmaLognormal2: TREEdit;
      pnlCbos: TPanel;
      lblPdfType: TLabel;
      cboPdfType: TComboBox;
      lblXAxis: TLabel;
      cboXAxis: TComboBox;  
  		cboYAxis: TComboBox;
      lblYAxis: TLabel;

      btnConvert: TButton;
      pnlUnspecified: TPanel;

      fraAcceptCancel: TFrameAcceptCancel;
      fraPointEditorGrid: TFramePointEditorGrid;   

      procedure cboPdfTypeChange(Sender: TObject);
		
			procedure reValueChanged( sender: TObject );
      procedure reExit( Sender: TObject);
      procedure reEnter(Sender: TObject);
			procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
			
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);

      procedure reKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

      procedure fraPointEditorGridbtnOKClick(Sender: TObject);
      procedure fraPointEditorGridbtnCancelClick(Sender: TObject);
      procedure fraPointEditorGridstgPointsEnter(Sender: TObject);

      procedure fraPointEditorGridstgPointsKeyDown(
        Sender: TObject;
        var Key: Word;
        Shift: TShiftState
      );

			procedure btnEditPiecewiseClick(Sender: TObject);

    protected
      // Restrictions for REL functions
      _minY: double;
      _maxY: double;

      // Function parameters
      _min: double;
      _max: double;
      _mode: double;
      _mean: double;
      _stddev: double;
      _alpha: double;
      _alpha2: double;
      _beta: double;
      _location: double;
      _scale: double;
      _shape: double;
      _zeta: double;
      _sigma: double;

      _unitsLocked: boolean;
      _nameVis: boolean;
      _nameLocked: boolean;

      _tempRecord: RPdfRecord;
      _tempPoints: RPointArray;

    	_myForm: TForm;
      _myEditor: TComponent;

      _editEnabled: boolean;

      _paramSetsList: TQStringObjectMap;

      _currentControl: TREEdit;

      _tempVal: string;

      _updateFunctionParams: TUpdateParamsEvent;
      _setParentMenuItemsEnabled: TObjFnVoid1Bool;

      _chartType: TChartType;
      
      _isReadOnly: boolean;

      procedure translateUI();

      procedure checkTempPoints();

      procedure showFraAcceptCancel( winControl: TWinControl );
      procedure hideFraAcceptCancel();

      procedure reChangeAccepted();
      procedure reChangeCanceled();

      procedure setReadOnly( val: boolean );
      function getReadOnly(): boolean;

      procedure updateChart();

      procedure setChartType( const val: TChartType );

			procedure forceValueRefresh();

      procedure showParamSet( boxName: string; enable: boolean );
      procedure initialize( AOwner: TComponent );
      procedure restoreValuesFromTempRecord();
      
      procedure updateFormFunctionParams();

      procedure populateCboPdfType();
      function pdfTypeFromCboPdfTypeItem(): TPdfType;
      function cboPdfTypeItemFromProdType( pdfType: TPdfType ): integer;

      procedure populateAxisCbo( cbx: TComboBox );
			procedure setEditControlsEnabled( val: boolean );
      procedure setupForEdit();
			function getName(): string;
			
      // property getters & setters
      procedure setMin( val: double );
      procedure setMode( val: double );
      procedure setMax( val: double );
      procedure setMean( val: double );
      procedure setStdDev( val: double );
      procedure setAlpha( val: double );
      procedure setAlpha2( val: double );
      procedure setBeta( val: double );
      procedure setLocation( val: double );
      procedure setScale( val: double );
      procedure setShape( val: double );
      procedure setZeta( val: double );
      procedure setSigma( val: double );

      function getMin(): double;
      function getMode(): double;
      function getMax(): double;
      function getMean(): double;
      function getStdDev(): double;
      function getAlpha(): double;
      function getAlpha2(): double;
      function getBeta(): double;
      function getLocation(): double;
      function getScale(): double;
      function getShape(): double;
      function getZeta(): double;
      function getSigma(): double;

      function getXUnits(): TChartUnitType;
      procedure setXUnits( val: TChartUnitType );
      function getUnitsLocked(): boolean;
      procedure setUnitsLocked( val: boolean );

      function getNameVis(): boolean;
      procedure setNameVis( val: boolean );
      function getNameLocked(): boolean;
      procedure setNameLocked( val: boolean );

      function getEditEnabled(): boolean;

      procedure resetStoredValues();

      procedure setYUnits( val: TChartUnitType );
      function getYUnits(): TChartUnitType;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

			procedure setForm( frm: TForm );

      procedure clear();

      procedure setUpdateParams( fn: TUpdateParamsEvent );
      procedure setSetParentMenuItemsEnabled( fn: TObjFnVoid1Bool );

      procedure setPoints( pnt: RPointArray );

      procedure copyChart( chart: TChartFunction; enableEdit: boolean = false );
      procedure copyPDF( pdf: TPdf; enableEdit: boolean = false );
      procedure copyRel( rel: TRelFunction );

			function createPdf(): TPdf;
      function createRel(): TRelFunction;

      property min: double read getMin write setMin;
      property max: double read getMax write setMax;
      property mode: double read getMode write setMode;
      property mean: double read getMean write setMean;
      property stddev: double read getStdDev write setStdDev;
      property alpha: double read getAlpha write setAlpha;
      property alpha2: double read getAlpha2 write setAlpha2;
      property beta: double read getBeta write setBeta;
      property location: double read getLocation write setLocation;
      property scale: double read getScale write setScale;
      property shape: double read getShape write setShape;
      property zeta: double read getZeta write setZeta;
      property sigma: double read getSigma write setSigma;

      property isReadOnly: boolean read getReadOnly write setReadOnly;

      property chartType: TChartType read _chartType write setChartType;

      property unitsLocked: boolean read getUnitsLocked write setUnitsLocked;
      property nameAlwaysVisible: boolean read getNameVis write setNameVis;
      property nameLocked: boolean read getNameLocked write setNameLocked;
      property editEnabled: boolean read getEditEnabled write setEditControlsEnabled;

			property xUnits: TChartUnitType read getXUnits write setXUnits;
      property yUnits: TChartUnitType read getYUnits write setYUnits;
      property minY: double read _minY write _minY;
      property maxY: double read _maxY write _maxY;
    
    end
  ;

  const
    DBFRAMEFUNCTIONPARAMS2: boolean = false; // Set to true to enable debugging messages for this unit.
  	DBFRAMEFUNCTIONPARAMS: boolean = false; // set to true to enable debugging messages for this unit.
    PREVIEWENABLED: boolean = false; // FIX ME: eliminate once preview and convert functions are in place.

implementation

{$R *.dfm}

	uses
		Math,
		TypInfo,  

		ARMath,
		myDialogs,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
		RegExpDefs,
		ControlUtils,
    I88n
	;


  constructor TFrameFunctionParams2.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
			initialize( AOwner );
			
      _setParentMenuItemsEnabled := nil;
      
      //_foo := false;
      _currentControl := nil;
    end
  ;


  procedure TFrameFunctionParams2.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:50:37 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/function_editor/FrameFunctionParams2.dfm
      // File date: Wed Apr 25 11:56:58 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          pnlUnspecified.Caption := tr( '(Select a function type)' );
          grpPoint.Caption := tr( 'Parameter' );
          lblValuePoint.Caption := tr( 'Value:' );
          grpGaussian.Caption := tr( 'Parameters' );
          lblMeanGaussian.Caption := tr( 'Mean:' );
          lblStdDevGaussian.Caption := tr( 'Standard deviation:' );
          grpTriang.Caption := tr( 'Parameters' );
          lblMinTriang.Caption := tr( 'Minimum:' );
          lblModeTriang.Caption := tr( 'Mode:' );
          lblMaxTriang.Caption := tr( 'Maximum:' );
          grpGamma.Caption := tr( 'Parameters' );
          lblAlphaGamma.Caption := tr( 'Alpha:' );
          lblBetaGamma.Caption := tr( 'Beta:' );
          grpExponential.Caption := tr( 'Parameter' );
          lblMeanExponential.Caption := tr( 'Mean:' );
          grpLogistic.Caption := tr( 'Parameters' );
          lblLocationLogistic.Caption := tr( 'Location:' );
          lblScaleLogistic.Caption := tr( 'Scale:' );
          grpPiecewise.Caption := tr( 'Parameters' );
          btnEditPiecewise.Caption := tr( 'View/Edit Points' );
          grpLoglogistic.Caption := tr( 'Parameters' );
          lblLocationLoglogistic.Caption := tr( 'Location:' );
          lblScaleLoglogistic.Caption := tr( 'Scale:' );
          lblShapeLoglogistic.Caption := tr( 'Shape:' );
          grpUniform.Caption := tr( 'Parameters' );
          lblMinUniform.Caption := tr( 'Minimum:' );
          lblMaxUniform.Caption := tr( 'Maximum:' );
          grpLognormal2.Caption := tr( 'Parameters' );
          lblZetaLognormal2.Caption := tr( 'Zeta:' );
          lblSigmaLognormal2.Caption := tr( 'Sigma'':' );
          grpBeta.Caption := tr( 'Parameters' );
          lblAlpha1Beta.Caption := tr( 'Alpha1:' );
          lblAlpha2Beta.Caption := tr( 'Alpha2:' );
          lblMinBeta.Caption := tr( 'Minimum:' );
          lblMaxBeta.Caption := tr( 'Maximum:' );
          btnConvert.Caption := tr( 'Convert to piecewise...' );
          lblPdfType.Caption := tr( 'Function type:' );
          lblXAxis.Caption := tr( 'X axis units:' );
          lblYAxis.Caption := tr( 'Y axis units:' );
        end
      ;

    end
  ;


  destructor TFrameFunctionParams2.destroy();
    begin
      // DO NOT delete the items stored in _paramSetsList: the list does not own the objects.
      // They will be freed when the form is closed.
    	freeAndNil( _paramSetsList );
      inherited destroy();
    end
  ;

  procedure TFrameFunctionParams2.initialize( AOwner: TComponent );
    var
      i, j: integer;
      re: TREEdit;
      grp: TGroupBox;
  	begin
      //-------------------------------------------
      // Used to be inherited
      //-------------------------------------------
      _myEditor := AOwner;

      pnlCbos.BevelInner := bvNone;
      pnlCbos.BevelOuter := bvNone;
      pnlCbos.Left := -8;
      pnlCbos.Top := -8;

      populateCboPdfType();
      populateAxisCbo( cboXAxis );

      with pnlParams do
        begin
          left := 224;
          top := 0;
          width := 121;
          height := 93;
          BevelInner := bvNone;
          BevelOuter := bvNone;
        end
      ;

			showParamSet( '(Unspecified)', false );

      setLength( _tempPoints, 0 );

      setEditControlsEnabled( false );

      unitsLocked := false;
      nameAlwaysVisible := false;
      nameLocked := false;

      reAlpha1Beta.InputExpression := RE_DECIMAL_INPUT;
      reAlpha2Beta.InputExpression := RE_DECIMAL_INPUT;
      reMinBeta.InputExpression := RE_DECIMAL_INPUT;
      reMaxBeta.InputExpression := RE_DECIMAL_INPUT;

      reAlphaGamma.InputExpression := RE_DECIMAL_INPUT;
      reBetaGamma.InputExpression := RE_DECIMAL_INPUT;

      reLocationLogistic.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      reScaleLogistic.InputExpression := RE_DECIMAL_INPUT;

      reLocationLoglogistic.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      reScaleLoglogistic.InputExpression := RE_DECIMAL_INPUT;
      reShapeLoglogistic.InputExpression := RE_DECIMAL_INPUT;

      reMaxTriang.InputExpression := RE_DECIMAL_INPUT;
      reMinTriang.InputExpression := RE_DECIMAL_INPUT;
      reModeTriang.InputExpression := RE_DECIMAL_INPUT;

      reMaxUniform.InputExpression := RE_DECIMAL_INPUT;
      reMinUniform.InputExpression := RE_DECIMAL_INPUT;

      reMeanExponential.InputExpression := RE_DECIMAL_INPUT;

      reMeanGaussian.InputExpression := RE_DECIMAL_INPUT;
      reStdDevGaussian.InputExpression := RE_DECIMAL_INPUT;

      reValuePoint.InputExpression := RE_DECIMAL_INPUT;

      reSigmaLognormal2.InputExpression := RE_DECIMAL_INPUT;
      reZetaLognormal2.InputExpression := RE_SIGNED_DECIMAL_INPUT;

      resetStoredValues();

      setupForEdit();      
      //-------------------------------------------
      
      _paramSetsList['Relational'] := grpPiecewise;

      self.Width := 153;
      pnlParams.Width := self.Width;
      pnlCbos.Width := self.Width;

      pnlCbos.Align := alTop;
      pnlParams.Align := alClient;

      horizCenterInside( btnConvert, self );
      showParamSet( '(Unspecified)', false );

      setEditControlsEnabled( true );

      unitsLocked := true;

      populateAxisCbo( cboYAxis );

      // FIX ME: a recursive search might be better.
      for i := 0 to pnlParams.ControlCount - 1 do
        begin
          if( pnlParams.Controls[i] is TGroupBox ) then
            begin
              grp := pnlParams.controls[i] as TGroupBox;
              for j := 0 to grp.ControlCount - 1 do
                begin
                  if( grp.Controls[j] is TREEdit ) then
                    begin
                      re := grp.Controls[j] as TREEdit;
                      re.OnEnter := reEnter;
                      re.OnExit := reExit;

                      re.OnKeyUp := reKeyUp;

                      re.Width := 113;
                    end
                  ;
                end
              ;
            end
          ;
        end
      ;

      _currentControl := nil;

      _updateFunctionParams := nil;
    end
  ;


  procedure TFrameFunctionParams2.setForm( frm: TForm );
  	begin
    	_myForm := frm;
    end
  ;


  procedure TFrameFunctionParams2.populateCboPdfType();
  	begin
      cboPdfType.Clear();

      _paramSetsList := TQStringObjectMap.create();

      cboPdfType.Items.Append( tr( '(Unspecified)' ) );
      _paramSetsList[ tr( '(Unspecified)' ) ] := grpUnspecified;

      cboPdfType.Items.Append( tr( 'Fixed value' ) );
      _paramSetsList[tr( 'Fixed value' ) ] := grpPoint;

      cboPdfType.Items.Append( tr( 'Uniform' ) );
      _paramSetsList[tr( 'Uniform' ) ] := grpUniform;

      cboPdfType.Items.Append( tr( 'Triangular' ) );
      _paramSetsList[tr( 'Triangular' ) ] := grpTriang;

      cboPdfType.Items.Append( tr( 'Piecewise (general)' ) );
      _paramSetsList[tr( 'Piecewise (general)' ) ] := grpPiecewise;

      cboPdfType.Items.Append( tr( 'Gaussian (normal)' ) );
      _paramSetsList[tr( 'Gaussian (normal)' ) ] := grpGaussian;

      cboPdfType.Items.Append( tr( 'Lognormal' ) );
      _paramSetsList[tr( 'Lognormal' ) ] := grpGaussian;

      cboPdfType.Items.Append( tr( 'Lognormal (2nd form)' ) );
      _paramSetsList[tr( 'Lognormal (2nd form)') ] := grpLognormal2;

      cboPdfType.Items.Append( tr( 'Gamma' ) );
      _paramSetsList[tr( 'Gamma') ] := grpGamma;

      cboPdfType.Items.Append( tr( 'Weibull' ) );
      _paramSetsList[tr( 'Weibull' ) ] := grpGamma;

      cboPdfType.Items.Append( tr( 'Pearson 5' ) );
      _paramSetsList[tr( 'Pearson 5' ) ] := grpGamma;

      cboPdfType.Items.Append( tr( 'Beta' ) );
      _paramSetsList[tr( 'Beta' ) ] := grpBeta;

      cboPdfType.Items.Append( tr( 'BetaPERT' ) );
      _paramSetsList[tr( 'BetaPERT' ) ] := grpTriang;

      cboPdfType.Items.Append( tr( 'Logistic' ) );
      _paramSetsList[tr( 'Logistic' ) ] := grpLogistic;

      cboPdfType.Items.Append( tr( 'Loglogistic' ) );
      _paramSetsList[tr( 'Loglogistic') ] := grpLogLogistic;

      cboPdfType.Items.Append( tr( 'Exponential' ) );
      _paramSetsList[tr( 'Exponential' )] := grpExponential;

      cboPdfType.ItemIndex := 0;

      // List the boxes // This apparently does nothing??
      (*
    	for i := 0 to _paramSetsList.Count-1 do
      	obj := _paramSetsList.itemAtIndex(i) as TGroupBox
      ;
      *)
    end
  ;


  procedure TFrameFunctionParams2.populateAxisCbo( cbx: TComboBox );
  	var
    	i: TChartUnitType;
 		begin
    	cbx.Clear();

      // REMEMBER: units type indices in the combo box are offset by one,
      // relative to TUnits!
      for i := low( TChartUnitType ) to high( TChartUnitType ) do
      	if i > low( TChartUnitType ) then cbx.Items.append( chartUnitTypeAsString(i) )
      ;

      cbx.Enabled := false; // !!!!
    end
  ;


  procedure TFrameFunctionParams2.restoreValuesFromTempRecord();
    begin
			self.mode := _tempRecord.mode;
			self.min := _tempRecord.min;
			self.max := _tempRecord.max;
			self.mean := _tempRecord.mean;
			self.stddev := _tempRecord.stddev;
      self.zeta := TPdfLognormal.calculateZeta( self.mean, self.stddev );
      self.sigma := TPdfLognormal.calculateSigmaPrime( self.mean, self.stddev );
			self.alpha := _tempRecord.alpha;
      self.alpha2 := _tempRecord.alpha2;
			self.beta := _tempRecord.beta;
			self.location := _tempRecord.location;
			self.scale := _tempRecord.scale;
			self.shape := _tempRecord.shape;
    end
  ;


  procedure TFrameFunctionParams2.resetStoredValues();
    begin
      _min := NaN;
      _mode := NaN;
      _max := NaN;
      _mean := NaN;
      _stddev := NaN;
      _alpha2 := NaN;
      _alpha := NaN;
      _beta := NaN;
      _location := NaN;
      _scale := NaN;
      _shape := NaN;
      _zeta := NaN;
      _sigma := NaN;

      setLength( _tempPoints, 0 );
    end
  ;


  procedure TFrameFunctionParams2.copyChart( chart: TChartFunction; enableEdit: boolean = false );
    begin
      case chart.chartType of
        CTPdf: copyPdf( chart as TPdf, enableEdit );
        CTRel: copyRel( chart as TRelFunction );
        else raise exception.Create( 'Unrecognized chart type in TFrameFunctionParams2.copyChart' );
      end;
    end
  ;

  procedure TFrameFunctionParams2.copyRel( rel: TRelFunction );
    begin
      _chartType := CTRel;

      if( 0 = length( rel.pointArray ) ) then
        begin
          // Create some fake points
          setLength( _tempPoints, 3 );

          _tempPoints[0].x := 0.0;
          _tempPoints[0].y := 100.0;

          _tempPoints[1].x := 3.5;
          _tempPoints[1].y := 50.0;

          _tempPoints[2].x := 7.0;
          _tempPoints[2].y := 25.0;
        end
      else if( 1 = length( rel.pointArray ) ) then
        begin
          setLength( _tempPoints, 2 );

          _tempPoints[1].x := _tempPoints[0].x + 1;
          _tempPoints[1].y := _tempPoints[0].y;
        end
      else
        _tempPoints := rel.pointArray
      ;

      showParamSet( 'Relational', false );
    end
  ;
  

  procedure TFrameFunctionParams2.clear();
  	begin
   		showParamSet( '(Unspecified)', false );
      cboPdfType.ItemIndex := -1;
      resetStoredValues();

      setEditControlsEnabled( false );// FIX ME: think about this.
    end
  ;


	procedure TFrameFunctionParams2.showParamSet( boxName: string; enable: boolean );
  	var
    	i, j: integer;
      obj: TGroupBox;
  	begin
      // Hide all boxes...
    	for i := 0 to _paramSetsList.Count-1 do
      	begin
        	obj := _paramSetsList.itemAtIndex(i) as TGroupBox;
          obj.Visible := false;
        end
      ;

      if( ( 'Relational' = boxName ) or ( _paramSetsList.contains( tr( boxName ) ) ) ) then
        begin
          if
            ( tr( 'Piecewise (general)' ) <> boxName )
          and
            ( 'Relational' <> boxName )
          then
            begin
              fraPointEditorGrid.visible := false;

              // Then show the appropriate one
              obj := _paramSetsList[ tr( boxName) ] as TGroupBox;
              obj.Align := alNone;
              horizCenterInside( obj, self );
              obj.Top := 0;
              obj.Visible := true;


              // Enable/disable child controls
              for j := 0 to obj.controlCount-1 do
                obj.controls[j].enabled := enable
              ;

              btnConvert.Visible := false; //('grpUnspecified' <> boxName );
            end
          else
            begin
              btnConvert.Visible := false;

              fraPointEditorGrid.Align := alClient;
              fraPointEditorGrid.visible := true;

              checkTempPoints();
              fraPointEditorGrid.setPoints( _tempPoints );
              
              updateFormFunctionParams();
            end
          ;
        end
      else
        raise exception.Create( 'Box name ' + boxName + ' does not exist' )
      ;
    end
  ;


	procedure TFrameFunctionParams2.reValueChanged( sender: TObject );
  	var
    	re: TREEdit;
      val: double;
      success: boolean;
  	begin
   		re := sender as TREEdit;
      val := -1;

      dbcout( '*** base reValueChanged', DBFRAMEFUNCTIONPARAMS );

      success := tryStrToFloat( re.Text, val );
      
      dbcout( 'Value of ' + re.Name + ' just changed to  ' + uiFloatToStr( val ), DBFRAMEFUNCTIONPARAMS );

      if
        ( reMinTriang = re )
      or
        ( reMinUniform = re )
      or
        ( reMinBeta = re )
      then
        min := val
      ;

      if
        ( reModeTriang = re )
      or
        ( reValuePoint = re )
      then
        mode := val
      ;

      if
        ( reMaxTriang = re )
      or
        ( reMaxUniform = re )
      or
        ( reMaxBeta = re )
      then
        max := val
      ;

      if
        ( reAlphaGamma = re )
      or
        ( reAlpha1Beta = re)
      then
        alpha := val
      ;

      if( reAlpha2Beta = re ) then alpha2 := val;

      if ( reBetaGamma = re ) then beta := val;

      if
        ( reMeanGaussian = re )
      or
        ( reMeanExponential = re )
      then
        begin
          mean := val;
          zeta := TPdfLognormal.calculateZeta( mean, stddev );
        end
      ;

      if( reStdDevGaussian = re ) then
        begin
          stddev := val;
          sigma := TPdfLognormal.calculateSigmaPrime( mean, stddev );
          zeta := TPdfLognormal.calculateZeta( mean, stddev );
        end
      ;


      if( reZetaLognormal2 = re ) then
        begin
          zeta := val;
          mean := TPdfLognormal.calculateMean( zeta, sigma );
          stddev := TPdfLognormal.calculateStddev( zeta, sigma );
        end
      ;

      if( reSigmaLognormal2 = re ) then
        begin
          sigma := val;
          stddev := TPdfLognormal.calculateStddev( zeta, sigma );
          mean := TPdfLognormal.calculateMean( zeta, sigma );
        end
      ;


      if
        ( reLocationLogistic = re)
      or
        ( reLocationLoglogistic = re )
      then
        location := val
      ;

      if
        ( reScaleLogistic = re )
      or
        ( reScaleLoglogistic = re )
      then
        scale := val
      ;

      if( reShapeLoglogistic = re ) then shape := val;

      if( not success ) then
      	re.text := ''
      ;
    end
  ;

  procedure TFrameFunctionParams2.setupForEdit();
  	begin
      forceValueRefresh();
      with _tempRecord do
        begin
          mode := self.mode;
          min := self.min;
          max := self.max;
          alpha := self.alpha;
          alpha2 := self.alpha2;
          beta := self.beta;
          location := self.location;
          scale := self.location;
          shape := self.shape;    
          mean := self.mean;
          stddev := self.stddev;

          pdfType := pdfTypeFromCboPdfTypeItem();
          pdfUnits := TChartUnitType( self.cboXAxis.ItemIndex + 1 );
          name := getName();
        end
      ;
    end
  ;



// Forces a refresh of all stored values from active/visible RegExp line editors.
procedure TFrameFunctionParams2.forceValueRefresh();
	var
    i, j: integer;
    obj: TGroupbox;
	begin
    // For every control on the form...
    for i := 0 to pnlParams.ControlCount-1 do
      begin

      	// ...is the control the visible, active group box?
        if( pnlParams.Controls[i] is TGroupBox ) and ( pnlParams.Controls[i].visible ) then
          begin

          	// If yes, check every control in that group box...
            obj := pnlParams.Controls[i] as TGroupbox;
            for j := 0 to obj.controlCount-1 do
              begin
              	// ...and store values from each of the controls that's an editor box.
                if( obj.controls[j] is TREEdit ) then reValueChanged( obj.Controls[j] );
              end
            ;
          end
      end
    ;
  end
;



procedure TFrameFunctionParams2.cboPdfTypeChange(Sender: TObject);
	var
  	key: string;
  begin
    key := cboPdfType.Items[cboPdfType.ItemIndex];

  	showParamSet( key, editEnabled );

    if( isNumber( _min ) ) then
      begin
        reMinTriang.Text := uiFloatToStr( _min );
        reMinUniform.Text := uiFloatToStr( _min );
        reMinBeta.Text := uiFloatToStr( _min );
      end
    else
      begin
        reMinTriang.Text := '';
        reMinUniform.Text := '';
        reMinBeta.Text := '';
      end
    ;

    if( isNumber( _mode ) ) then
      begin
        reModeTriang.Text := uiFloatToStr( _mode );
        reValuePoint.Text := uiFloatToStr( _mode );
      end
    else
      begin
        reModeTriang.Text := '';
        reValuePoint.text := '';
      end
    ;

    if( isNumber( _max ) ) then
      begin
        reMaxTriang.Text := uiFloatToStr( _max );
        reMaxUniform.Text := uiFloatToStr( _max );
        reMaxBeta.Text := uiFloatToStr( _max );
      end
    else
      begin
        reMaxTriang.Text := '';
        reMaxUniform.Text := '';
        reMaxBeta.Text := '';
      end
    ;

    if( isNumber( _mean ) ) then
      begin
        reMeanGaussian.Text := uiFloatToStr( _mean );
        reMeanExponential.Text := uiFloatToStr( _mean );
      end
    else
      begin
        reMeanGaussian.Text := '';
        reMeanExponential.Text := '';
      end
    ;

    if( isNumber( _stddev ) ) then
      reStddevGaussian.Text := uiFloatToStr( _stddev ) else reStddevGaussian.Text := ''
    ;

    if( isNumber( _alpha ) ) then
      begin
        reAlpha1Beta.Text := uiFloatToStr( _alpha );
        reAlphaGamma.Text := uiFloatToStr( _alpha );
      end
    else
      begin
        reAlpha1Beta.Text := '';
        reAlphaGamma.Text := '';
      end
    ;

    if( isNumber( _alpha2 ) ) then
      reAlpha2Beta.Text := uiFloatToStr( _alpha2 )
    else
      reAlpha2Beta.Text := '';
    ;

    if( isNumber( _beta ) ) then
      reBetaGamma.Text := uiFloatToStr( _beta )
    else
      reBetaGamma.Text := ''
    ;


    if( isNumber( _location ) ) then
      begin
        reLocationLogLogistic.Text := uiFloatToStr( _location );
        reLocationLogistic.Text := uiFloatToStr( _location );
      end
    else
      begin
        reLocationLogLogistic.Text := '';
        reLocationLogistic.Text := '';
      end
    ;

    if( isNumber( _scale ) ) then
      begin
        reScaleLogistic.Text := uiFloatToStr( _scale );
        reScaleLoglogistic.Text := uiFloatToStr( _scale );
      end
    else
      begin
        reScaleLogistic.Text := '';
        reScaleLoglogistic.Text := '';
      end
    ;

    if( isNumber( _shape ) ) then
      reShapeLoglogistic.Text := uiFloatToStr( _shape ) else reShapeLoglogistic.Text := ''
    ;

    if( isNumber( _zeta ) ) then
      reZetaLognormal2.Text := uiFloatToStr( _zeta ) else reZetaLognormal2.Text := ''
    ;

    if( isNumber( _sigma ) ) then
      reSigmaLognormal2.Text := uiFloatToStr( _sigma ) else reSigmaLognormal2.Text := ''
    ;

    updateFormFunctionParams();
    if( nil <> @_setParentMenuItemsEnabled ) then _setParentMenuItemsEnabled( true );
  end
;



procedure TFrameFunctionParams2.showFraAcceptCancel( winControl: TWinControl );
  begin
    fraAcceptCancel.Parent := winControl.Parent;

    winControl.Width := 65;
    fraAcceptCancel.Top := winControl.Top;
    fraAcceptCancel.Left := winControl.Left + winControl.Width + 1;

    fraAcceptCancel.Visible := true;

    cboPdfType.Enabled := false;
    cboXAxis.Enabled := false;
    cboYAxis.Enabled := false;
    btnConvert.Enabled := false;
  end
;


procedure TFrameFunctionParams2.hideFraAcceptCancel();
  begin
    fraAcceptCancel.Visible := false;

    cboPdfType.Enabled := true;
    cboXAxis.Enabled := not( unitsLocked );
    cboYAxis.Enabled := not( unitsLocked );

    updateFormFunctionParams();
  end
;


procedure TFrameFunctionParams2.reEnter( Sender: TObject);
  var
    winControl: TREEdit;
  begin
    if( nil <> @_setParentMenuItemsEnabled ) then
      _setParentMenuItemsEnabled( false )
    ;

    if( fraAcceptCancel.Visible ) then
      reChangeAccepted()
    ;

    winControl := sender as TREEdit;

    dbcout( 're entered: ' + winControl.Name, DBFRAMEFUNCTIONPARAMS2 );

    _tempVal := winControl.Text;

    _currentControl := winControl;

    //if( not( _foo ) ) then
      showFraAcceptCancel( winControl )
    (*
    else
      begin
        fraAcceptCancel.Visible := false;
        //cboPdfType.SetFocus();
        pnlCbos.SetFocus();
        _foo := false;
      end
    *)
    ;
  end
;


procedure TFrameFunctionParams2.reExit( Sender: TObject);
  var
    ctrl: TREEdit;
  begin
    ctrl := sender as TREEdit;
    dbcout( 'Exiting ' + ctrl.Name, DBFRAMEFUNCTIONPARAMS2 );
    ctrl.Width := 113;

    if( nil <> @_setParentMenuItemsEnabled ) then
      _setParentMenuItemsEnabled( true )
    ;
  end
;


procedure TFrameFunctionParams2.reChangeAccepted();
  begin

    if( nil <> _currentControl ) then
      begin
        dbcout( 'Change accepted in ' + _currentControl.Name, DBFRAMEFUNCTIONPARAMS2 );
        reValueChanged( _currentControl );
      end
    ;

    updateFormFunctionParams();
  end
;


procedure TFrameFunctionParams2.reChangeCanceled();
  begin
    dbcout( 'Cancel clicked', DBFRAMEFUNCTIONPARAMS2 );
    dbcout( 'Change rejected in ' + _currentControl.Name, DBFRAMEFUNCTIONPARAMS2 );
    _currentControl.Text := _tempVal;
  end
;


procedure TFrameFunctionParams2.fraAcceptCancelbtnAcceptClick( Sender: TObject );
  begin
    inherited;
    dbcout( 'Accept clicked', DBFRAMEFUNCTIONPARAMS2 );
    hideFraAcceptCancel();
    dbcout( 'Accepting change in ' + _currentControl.Name, DBFRAMEFUNCTIONPARAMS2 );
    reChangeAccepted();
    _currentControl.Parent.SetFocus();
  end
;


procedure TFrameFunctionParams2.fraAcceptCancelbtnCancelClick( Sender: TObject );
  begin
    inherited;
    hideFraAcceptCancel();
    dbcout( 'Canceling change in ' + _currentControl.Name, DBFRAMEFUNCTIONPARAMS2 );
    reChangeCanceled();
    _currentControl.Parent.SetFocus();
  end
;


procedure TFrameFunctionParams2.reKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
  begin
    if( 13 = key ) then
      begin
        _currentControl := sender as TREEdit;
        dbcout( 'Enter in ' + _currentControl.Name, DBFRAMEFUNCTIONPARAMS2 );
        fraAcceptCancelbtnAcceptClick( nil );
      end
    else if( 27 = key ) then
      begin
        _currentControl := sender as TREEdit;
        dbcout( 'Cancel in ' + _currentControl.Name, DBFRAMEFUNCTIONPARAMS2 );
        fraAcceptCancelbtnCancelClick( nil );
      end
    ;

    inherited;
  end
;


procedure TFrameFunctionParams2.setUpdateParams( fn: TUpdateParamsEvent );
  begin
    _updateFunctionParams := fn;
    updateFormFunctionParams();
  end
;


procedure TFrameFunctionParams2.setSetParentMenuItemsEnabled( fn: TObjFnVoid1Bool );
  begin
    _setParentMenuItemsEnabled := fn;
    fraPointEditorGrid.setSetParentMenuItemsEnabled( fn );
  end
;


(*
procedure TFrameFunctionParams2.cboPdfTypeChange(Sender: TObject);
  begin
    inherited;
    updateFormFunctionParams();
    if( nil <> @_setParentMenuItemsEnabled ) then _setParentMenuItemsEnabled( true );
  end
;
*)

procedure TFrameFunctionParams2.updateFormFunctionParams();
  var
    fn: TChartFunction;
  begin
    case chartType of
      CTPdf:
        begin
          fn := createPDF();
          btnConvert.Enabled := ( not( fraAcceptCancel.Visible ) and ( (fn as TPdf).canConvertToPiecewise ) );
        end
      ;
      CTRel:
        begin
          fn := createRel();
          btnConvert.Enabled := false;
        end
      ;
      else
        fn := nil
      ;
    end;

    dbcout( 'Trying to debug function:', DBFRAMEFUNCTIONPARAMS2 );
    if( DBFRAMEFUNCTIONPARAMS2 ) then if( nil <> fn ) then fn.debug();
    dbcout( 'Done debugging', DBFRAMEFUNCTIONPARAMS2 );

    if( nil <> @_updateFunctionParams ) then _updateFunctionParams( self, fn );

    freeAndNil( fn );
  end
;



  function TFrameFunctionParams2.createRel(): TRelFunction;
    var
      rel: TRelFunction;
    begin
      rel := TRelFunction.create( _tempPoints, xUnits, yUnits );
      rel.name := getName();
      result := rel;
    end
  ;


  procedure TFrameFunctionParams2.fraPointEditorGridbtnOKClick( Sender: TObject );
    begin
      inherited;
      fraPointEditorGrid.btnOKClick(Sender);

      cboPdfType.Enabled := true;
      cboXAxis.Enabled := not( unitsLocked );
      cboYAxis.Enabled := not( unitsLocked );

      updateChart();
    end
  ;


  procedure TFrameFunctionParams2.updateChart();
    var
      i: integer;
    begin
      setLength( _tempPoints, fraPointEditorGrid.stgPoints.RowCount - 1 ); // subtract 1 for the header row

      if( CTRel = _chartType ) then
        begin
          // Check min and max constraints.

          for i := 0 to length( _tempPoints ) - 1 do
            begin
              if
                ( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[1, i+1] ) < minY )
              or
                ( ( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[1, i+1] ) > maxY ) and ( maxY > 0.0 ) )
              or
                ( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[0, i+1] ) < 0 ) // all X values must be 0 or greater
              then
                begin
                  msgOK(
                    tr( 'At least one value is outside the range allowed for this function' ) + '.'
                     //+ '(' +  uiFloatToStr( minY ) + ' ' + tr( 'to' ) + ' ' + uiFloatToStr( maxY ) + ').'
                     + '  ' + tr( 'All such values will be adjusted.' ),
                    tr( 'Invalid values' ),
                    IMGWarning,
                    _myForm
                  );
                  break;
                end
              ;
            end
          ;
        end
      ;

      for i := 0 to length( _tempPoints ) - 1 do
        begin
          _tempPoints[i].x := myStrToFloat( fraPointEditorGrid.stgPoints.Cells[0, i+1] );
          _tempPoints[i].y := myStrToFloat( fraPointEditorGrid.stgPoints.Cells[1, i+1] );

          if( CTRel = _chartType ) then
            begin
              // Check X restrictions for REL functions
              if( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[0, i+1] ) < 0 ) then
                begin
                  _tempPoints[i].x := 0;
                  fraPointEditorGrid.stgPoints.Cells[0, i+1] := uiFloatToStr( 0 );
                end
              ;

              // Check Y restrictions for REL functions
              if( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[1, i+1] ) < minY ) then
                begin
                  _tempPoints[i].y := minY;
                  fraPointEditorGrid.stgPoints.Cells[1, i+1] := uiFloatToStr( minY );
                end
              else if( ( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[1, i+1] ) > maxY ) and ( maxY > 0.0 ) ) then
                begin
                  _tempPoints[i].y := maxY;
                  fraPointEditorGrid.stgPoints.Cells[1, i+1] := uiFloatToStr( maxY );
                end
              ;
            end
          ;
        end
      ;

      if( CTPdf = _chartType ) then
        begin
          if( TPdfPiecewise.standardize( _tempPoints ) ) then
            fraPointEditorGrid.setPoints( _tempPoints )
          ;
        end
      ;

      updateFormFunctionParams();
    end
  ;


  procedure TFrameFunctionParams2.fraPointEditorGridbtnCancelClick( Sender: TObject );
    begin
      inherited;
      fraPointEditorGrid.btnCancelClick(Sender);

      cboPdfType.Enabled := true;
      cboXAxis.Enabled := not( unitsLocked );
      cboYAxis.Enabled := not( _unitsLocked );

      // If the user clicked cancel, restore the points
      // the way the were before they were messed with.
      checkTempPoints();
      fraPointEditorGrid.setPoints( _tempPoints );
    end
  ;


  procedure TFrameFunctionParams2.fraPointEditorGridstgPointsEnter( Sender: TObject );
    begin
      inherited;
      fraPointEditorGrid.stgPointsEnter( sender );

      cboPdfType.Enabled := false;
      cboXAxis.Enabled := false;
      cboYAxis.Enabled := false;
    end
  ;


  procedure TFrameFunctionParams2.checkTempPoints();
    begin
      if( ( CTPdf = _chartType ) and ( 3 > length( _tempPoints ) ) ) then
        begin
          // Create some fake points
          setLength( _tempPoints, 3 );

          _tempPoints[0].x := 0.0;
          _tempPoints[0].y := 0.0;

          _tempPoints[1].x := 3.0;
          _tempPoints[1].y := 0.4;

          _tempPoints[2].x := 5.0;
          _tempPoints[2].y := 0.0;
        end
      else if( CTRel = _chartType ) then
        begin
          if( 0 = length( _tempPoints ) ) then
            begin
              // Create some fake points
              setLength( _tempPoints, 3 );

              _tempPoints[0].x := 0.0;
              _tempPoints[0].y := 100.0;

              _tempPoints[1].x := 3.5;
              _tempPoints[1].y := 50.0;

              _tempPoints[2].x := 7.0;
              _tempPoints[2].y := 25.0;
            end
          else if( 1 = length( _tempPoints ) ) then
            begin
              setLength( _tempPoints, 2 );

              _tempPoints[1].x := _tempPoints[0].x + 1;
              _tempPoints[1].y := _tempPoints[0].y;
            end
          ;

          // 2 or more points are fine: don't do anything.
        end
      ;
    end
  ;

procedure TFrameFunctionParams2.setPoints( pnt: RPointArray );
  begin
    setLength( _tempPoints, 0 );
    _tempPoints := pnt;
    fraPointEditorGrid.setPoints( _tempPoints );
  end
;


procedure TFrameFunctionParams2.fraPointEditorGridstgPointsKeyDown(
    Sender: TObject;
    var Key: Word;
    Shift: TShiftState
  );
begin
  dbcout( key, DBFRAMEFUNCTIONPARAMS2 );
  inherited;

  dbcout2( 'Key down: ' + intToStr( Key ) );

  // FIX ME??? For some reason, keyDown doesn't seem to work in fraPointEditorGrid,
  // even though that's where the tab and down arrow key handling should logically take place.
  // These keys are handled here as a work-around.
  if( key in [9, 40] ) then
    begin
      if( ( fraPointEditorGrid.stgPoints.Col = fraPointEditorGrid.stgPoints.ColCount - 1 ) and ( fraPointEditorGrid.stgPoints.Row = fraPointEditorGrid.stgPoints.RowCount - 1 ) ) then
        begin
          dbcout2( 'Tabbed from last cell' );
          fraPointEditorGrid.stgPoints.RowCount := fraPointEditorGrid.stgPoints.RowCount + 1;
          if( 1 < fraPointEditorGrid.stgPoints.RowCount ) then
            begin
              fraPointEditorGrid.stgPoints.Cells[0, fraPointEditorGrid.stgPoints.RowCount - 1 ] := uiFloatToStr( myStrToFloat( fraPointEditorGrid.stgPoints.Cells[0, fraPointEditorGrid.stgPoints.RowCount - 2 ] ) + 1 );
              fraPointEditorGrid.stgPoints.Cells[1, fraPointEditorGrid.stgPoints.RowCount - 1 ] := fraPointEditorGrid.stgPoints.Cells[1, fraPointEditorGrid.stgPoints.RowCount - 2 ];
            end
          ;
        end
      ;
    end
  else if( 13 = key ) then
    begin
      fraPointEditorGrid.SetFocus();
      fraPointEditorGridbtnOKClick( nil );
    end
  else if( 27 = key ) then
    begin
      fraPointEditorGrid.SetFocus();
      fraPointEditorGridbtnCancelClick( nil );
    end
  ;
end;


procedure TFrameFunctionParams2.setReadOnly( val: boolean );
  begin
    _isReadOnly := val;

    setChildrenEnabled( self, not( _isReadOnly ), true );

    // Even though children are enabled, these two controls shouldn't be.
    fraPointEditorGrid.btnOK.Enabled := false;
    fraPointEditorGrid.btnCancel.Enabled := false;

    // If units are locked, these shouldn't be enabled either.
    // Otherwise the user can mess with stuff...
    if( unitsLocked ) then
      begin
        cboXAxis.Enabled := false;
        cboYAxis.Enabled := false;
      end
    ;
  end
;


function TFrameFunctionParams2.getReadOnly(): boolean;
  begin
    result := _isReadOnly;
  end
;

procedure TFrameFunctionParams2.setChartType( const val: TChartType );
  begin
    _chartType := val;
    
    case val of
      CTRel:
        begin
          showParamSet( tr( 'Piecewise (general)' ), not( _isReadOnly ) );  // For reasons that aren't entirely clear, 'Piecewise (general)' does need to be translated...

          lblYAxis.Visible := true;
          lblYAxis.Top := lblPdfType.Top;
          lblYAxis.Left := lblPdfType.Left;

          cboYAxis.Visible := true;
          cboYAxis.Top := cboPdfType.Top;
          cboYAxis.Left := cboPdfType.Left;

          lblPdfType.Visible := false;
          cboPdfType.Visible := false;
        end
      ;
      else
        begin
          showParamSet( '(Unspecified)', not( _isReadOnly ) );

          lblYAxis.Visible := false;
          cboYAxis.Visible := false;

          lblPdfType.Visible := true;
          cboPdfType.Visible := true;
        end
      ;
    end;
  end
;


function TFrameFunctionParams2.getYUnits(): TChartUnitType;
  begin
    result := TChartUnitType( cboYAxis.ItemIndex + 1 );
  end
;


procedure TFrameFunctionParams2.setYUnits( val: TChartUnitType );
  begin
    cboYAxis.ItemIndex := ord(val) - 1;
  end
;


procedure TFrameFunctionParams2.setUnitsLocked( val: boolean );
  begin
    _unitsLocked := val;
    cboXAxis.Enabled := not( _unitsLocked );
    cboYAxis.Enabled := not( _unitsLocked );
  end
;

  procedure TFrameFunctionParams2.btnEditPiecewiseClick(Sender: TObject);
    begin
      // do nothing
    end
  ;


	procedure TFrameFunctionParams2.setEditControlsEnabled( val: boolean );
  	var
    	i, j: integer;
      obj: TGroupbox;
  	begin
    	cboPdfType.Enabled := val;

      if( unitsLocked ) then
      	cboXAxis.Enabled := false
      else
      	cboXAxis.Enabled := val
      ;

      for i := 0 to _paramSetsList.count-1 do
      	begin
       		obj := _paramSetsList.itemAtIndex(i) as TGroupBox;
          obj.enabled := val;
          for j := 0 to obj.controlCount-1 do
          	begin
            	obj.controls[j].enabled := val;
            end
          ;
        end
      ;

      _editEnabled := val;
    end
  ;


	function TFrameFunctionParams2.getName(): string;
		begin
			result := '';
		end
	;


	procedure TFrameFunctionParams2.copyPDF( pdf: TPdf; enableEdit: boolean = false );
  	var
      triang: TPdfTriangular;
      point: TPdfPoint;
      gauss: TPdfGaussian;
      pieces: TPdfPiecewise;
      uniform: TPdfUniform;
      beta: TPdfBeta;
      betaPERT: TPdfBetaPert;
      gamma: TPdfGamma;
      weibull: TPdfWeibull;
      exp: TPdfExponential;
      pearson: TPdfPearson5;
      logistic: TPdfLogistic;
      loglogistic: TPdfLoglogistic;
      lognormal: TPdfLogNormal;
  	begin
      if( nil = pdf ) then exit;

//      grp := nil;

      //if( enableEdit ) then cboPdfType.Enabled := true;
      
    	if( pdf.pdfType = PdfUndefined ) then
        begin
          showParamSet( '(Unspecified)', false );
          exit;
        end
      ;

      if( editEnabled ) then
      	begin
          msgOK(
          	tr( 'Please finish editing the current probability density function before attempting this operation.' ),
          	tr( 'Cannot set PDF' ),
            IMGCritical,
            _myForm
    			);
          cboPdfType.SetFocus();
        	exit;
      	end
      ;

      if( enableEdit ) then
      	setupForEdit()
      ;

      cboPdfType.ItemIndex := cboPdfTypeItemFromProdType( pdf.pdfType );
      cboXAxis.ItemIndex := ord( pdf.xUnits ) - 1; // Remember the offset!

      resetStoredValues();

    	case pdf.pdfType of
      	PdfTriangular:
        	begin
         		showParamSet( 'Triangular', false );
//            grp := grpTriang;

            triang := TPdfTriangular( pdf );

            _min := triang.min;
            reMinTriang.Text := uiFloatToStr( triang.min );
            _mode := triang.mode;
            reModeTriang.Text := uiFloatToStr( triang.mode );
            _max := triang.max;
            reMaxTriang.Text := uiFloatToStr( triang.max );

            setLength( _tempPoints, 0 );
          end
        ;
        PdfPoint:
        	begin
         		showParamSet( 'Fixed value', false );
//            grp := grpPoint;

            point := TPdfPoint( pdf );

            _mode := point.value;
            reValuePoint.text := uiFloatToStr( point.value );
          end
        ;
      	PdfGaussian:
        	begin
          	showParamSet( 'Gaussian (normal)', false );
//            grp := grpGaussian;

            gauss := TPdfGaussian( pdf );

            _mean := gauss.mean;
            reMeanGaussian.Text := uiFloatToStr( gauss.mean );
            _stddev := gauss.stddev;
            reStddevGaussian.Text := uiFloatToStr( gauss.stddev );
          end
        ;
        PdfPiecewise:
        	begin
            // Build the function before showing the parameter set,
            // to allow the various pieces to get the point values right.
            pieces := TPdfPiecewise( pdf );

            if( 3 > length( pieces.pointArray ) ) then
              begin
                _tempPoints := nil;

                setLength( _tempPoints, 3 );

                _tempPoints[0].x := 0.0;
                _tempPoints[0].y := 0.0;

                _tempPoints[1].x := 3.0;
                _tempPoints[1].y := 0.4;

                _tempPoints[2].x := 5.0;
                _tempPoints[2].y := 0.0;
              end
            else
              _tempPoints := pieces.pointArray
            ;

          	showParamSet( tr( 'Piecewise (general)' ), false ); // For reasons that aren't entirely clear, 'Piecewise (general)' does need to be translated.
//            grp := grpPiecewise;
          end
        ;
      PdfUniform:
        	begin
          	showParamSet( 'Uniform', false );
//            grp := grpUniform;

            uniform := TPdfUniform( pdf );

            _min := uniform.min;
            reMinUniform.Text := uiFloatToStr( uniform.min );
            _max := uniform.max;
            reMaxUniform.Text := uiFloatToStr( uniform.max );
          end
        ;
      PdfBetaPERT:
        	begin
          	showParamSet( 'BetaPERT', false );
//            grp := grpTriang;

            betaPERT := TPdfBetaPERT( pdf );

            _min := betaPERT.min;
            reMinTriang.Text := uiFloatToStr( betaPERT.min );
            _mode := betaPERT.mode;
            reModeTriang.Text := uiFloatToStr( betaPERT.mode );
            _max := betaPERT.max;
            reMaxTriang.Text := uiFloatToStr( betaPERT.max );
          end
        ;
      PdfGamma:
        	begin
          	showParamSet( 'Gamma', false );
//            grp := grpGamma;

            gamma := TPdfGamma( pdf );

            _alpha := gamma.alpha;
            reAlphaGamma.Text := uiFloatToStr( gamma.alpha );
            _beta := gamma.beta;
            reBetaGamma.Text := uiFloatToStr( gamma.beta );
          end
        ;
      PdfWeibull:
        	begin
          	showParamSet( 'Weibull', false );
//            grp := grpGamma;

            weibull := TPdfWeibull( pdf );

            _alpha := weibull.alpha;
            reAlphaGamma.Text := uiFloatToStr( weibull.alpha );
            _beta := weibull.beta;
            reBetaGamma.Text := uiFloatToStr( weibull.beta );
          end
        ;
      PdfExponential:
        	begin
          	showParamSet( 'Exponential', false );
 //           grp := grpExponential;

            exp := TPdfExponential( pdf );

            _mean := exp.mean;
            reMeanExponential.Text := uiFloatToStr( exp.mean );
          end
        ;
      PdfPearson5:
        	begin
          	showParamSet( 'Gamma', false );
//            grp := grpGamma;

            pearson := TPdfPearson5( pdf );

            _alpha := pearson.alpha;
            reAlphaGamma.Text := uiFloatToStr( pearson.alpha );
            _beta := pearson.beta;
            reBetaGamma.Text := uiFloatToStr( pearson.beta );
          end
        ;
      PdfLogistic:
        	begin
          	showParamSet( 'Logistic', false );
//            grp := grpLogistic;

            logistic := TPdfLogistic( pdf );

            _location := logistic.location;
            reLocationLogistic.Text := uiFloatToStr( logistic.location );
            _scale := logistic.scale;
            reScaleLogistic.Text := uiFloatToStr( logistic.scale );
          end
        ;
      PdfLogLogistic:
        	begin
          	showParamSet( 'Loglogistic', false );
//            grp := grpLogLogistic;

            loglogistic := TPdfLogLogistic( pdf );

            _location := loglogistic.location;
            reLocationLoglogistic.Text := uiFloatToStr( loglogistic.location );
            _scale := loglogistic.scale;
            reScaleLoglogistic.Text := uiFloatToStr( loglogistic.scale );
            _shape := loglogistic.shape;
            reShapeLoglogistic.Text := uiFloatToStr( loglogistic.shape );
          end
        ;
      PdfLognormal:
        	begin
          	showParamSet( 'Lognormal', false );
//            grp := grpGaussian;

            lognormal := TPdfLognormal( pdf );

            _mean := lognormal.mean;
            reMeanGaussian.Text := uiFloatToStr( lognormal.mean );
            _stddev := lognormal.stddev;
            reStddevGaussian.Text := uiFloatToStr( lognormal.stddev );

            _zeta := lognormal.zeta;
            reZetaLognormal2.Text := uiFloatToStr( lognormal.zeta );
            _sigma := lognormal.sigma;
            reSigmaLognormal2.Text := uiFloatToStr( lognormal.sigma );
          end
        ;
      PdfBeta:
        	begin
          	showParamSet( 'Beta', false );
//            grp := grpBeta;

            beta := TPdfBeta( pdf );
            _min := beta.min;

             reMinBeta.Text := uiFloatToStr( beta.min );
            _max := beta.max;
            reMaxBeta.Text := uiFloatToStr( beta.max );
            _alpha := beta.alpha1;
            reAlpha1Beta.Text := uiFloatToStr( beta.alpha1 );
            _alpha2 := beta.alpha2;
            reAlpha2Beta.Text := uiFloatToStr( beta.alpha2 );
          end
        ;
      else
      	showMessage( 'Unrecognized PDF type in TFrameFunctionParamsBase.setPDF()' );
      end;

      if( enableEdit ) then
        begin
      	  setEditControlsEnabled( true );
          //if( nil <> grp ) then grp.SetFocus();
          //cboPdfType.SetFocus();
        end
      else
      	setEditControlsEnabled( false )
      ;


    end
  ;
	
	
  function TFrameFunctionParams2.createPDF(): TPdf;
    var
      pdf: TPdf;
      logn: TPdfLognormal;
    begin
      case cboPdfType.itemIndex of
        //0:  pdf := TPdf.create();
        1:  pdf := TPdfPoint.create( mode, xUnits );
        2:  pdf := TPdfUniform.create( min, max, xUnits ) ;
        3:  pdf := TPdfTriangular.create( min, mode, max, xUnits );
        4:  pdf := TPdfPiecewise.create( _tempPoints, xUnits );
        5:  pdf := TPdfGaussian.create( mean, stddev, xUnits );
        6:  pdf := TPdfLognormal.create( mean, stddev, xUnits );
        7:
          begin
            logn := TPdfLognormal.create();
            logn.xUnits := xUnits;
            logn.zeta := zeta;
            logn.sigma := sigma;
            pdf := logn;
          end
        ;
        8:  pdf := TPdfGamma.create( alpha, beta, xUnits );
        9:  pdf := TPdfWeibull.create( alpha, beta, xUnits );
        10: pdf := TPdfPearson5.create( alpha, beta, xUnits );
        11: pdf := TPdfBeta.create( alpha, alpha2, min, max, xUnits );
        12: pdf := TPdfBetaPERT.create( min, mode, max, xUnits );
        13: pdf := TPdfLogistic.create( location, scale, xUnits );
        14: pdf := TPdfLoglogistic.create( location, scale, shape, xUnits );
        15:  pdf := TPdfExponential.create( mean, xUnits );
        else
          raise exception.create( 'Unsupported pdf type ' + intToStr( cboPdfType.ItemIndex ) + ' in TFrameFunctionParams2.createPDF()' )
        ;
      end;

      pdf.name := getName();

      result := pdf;
    end
  ;

  function TFrameFunctionParams2.pdfTypeFromCboPdfTypeItem(): TPdfType;
    begin
      case cboPdfType.itemIndex of
        0:    result := PdfUndefined;
        1:    result := PdfPoint;
        2:    result := PdfUniform;
        3:    result := PdfTriangular;
        4:    result := PdfPiecewise;
        5:    result := PdfGaussian;
        6, 7: result := PdfLognormal;
        8:    result := PdfGamma;
        9:    result := PdfWeibull;
        10:   result := PdfPearson5;
        11:   result := PdfBeta;
        12:   result := PdfBetaPERT;
        13:   result := PdfLogistic;
        14:   result := PdfLogLogistic;
        15:   result := PdfExponential;
        else result := PdfUndefined;
      end;
    end
  ;


  function TFrameFunctionParams2.cboPdfTypeItemFromProdType( pdfType: TPdfType ): integer;
    begin
      case pdfType of
        PdfUndefined: result := 0;
        PdfPoint: result := 1;
        PdfUniform: result := 2;
        PdfTriangular: result := 3;
        PdfPiecewise: result := 4;
        PdfGaussian: result := 5;
        PdfLognormal: result := 6;
        //PdfLognormal2: result := 7; // This shouldn't happen.  The second form of the Lognormal is now indistinguishable from the main form.
        PdfGamma: result := 8;
        PdfWeibull: result := 9;
        PdfPearson5: result := 10;
        PdfBeta: result := 11;
        PdfBetaPERT: result := 12;
        PdfLogistic: result := 13;
        PdfLogLogistic: result := 14;
        PdfExponential: result := 15;
      else
        begin
          raise exception.create( 'Illegal value (' + intToStr( ord(pdfType) ) + ') in TFrameFunctionParamsBase.cboPdfTypeItemFromProdType' );
          result := -1;
        end
      ;
      end;
    end
  ;
	
	

	function TFrameFunctionParams2.getXUnits(): TChartUnitType;
  	begin
    	result := TChartUnitType( cboXAxis.ItemIndex + 1 );
    end
  ;


  procedure TFrameFunctionParams2.setXUnits( val: TChartUnitType );
  	begin
    	cboXAxis.ItemIndex := ord(val) - 1;
    end
  ;

// Properties
  procedure TFrameFunctionParams2.setMin( val: double ); begin _min := val; end;
  procedure TFrameFunctionParams2.setMode( val: double ); begin _mode := val; end;
  procedure TFrameFunctionParams2.setmax( val: double ); begin _max := val; end;
  procedure TFrameFunctionParams2.setMean( val: double ); begin _mean := val; end;
  procedure TFrameFunctionParams2.setStdDev( val: double ); begin _stddev := val; end;
  procedure TFrameFunctionParams2.setAlpha( val: double ); begin _alpha := val; end;
  procedure TFrameFunctionParams2.setAlpha2( val: double ); begin _alpha2 := val; end;
  procedure TFrameFunctionParams2.setBeta( val: double ); begin _beta := val; end;
  procedure TFrameFunctionParams2.setLocation( val: double ); begin _location := val; end;
  procedure TFrameFunctionParams2.setScale( val: double ); begin _scale := val; end;
  procedure TFrameFunctionParams2.setShape( val: double ); begin _shape := val; end;
  procedure TFrameFunctionParams2.setZeta( val: double ); begin _zeta := val; end;
  procedure TFrameFunctionParams2.setSigma( val: double ); begin _sigma := val; end;

  function TFrameFunctionParams2.getMin(): double; begin Result := _min; end;
  function TFrameFunctionParams2.getMode(): double; begin Result := _mode; end;
  function TFrameFunctionParams2.getMax(): double; begin Result := _max; end;
  function TFrameFunctionParams2.getMean(): double; begin Result := _mean; end;
  function TFrameFunctionParams2.getStdDev(): double; begin Result := _stddev; end;
  function TFrameFunctionParams2.getAlpha(): double; begin result := _alpha; end;
  function TFrameFunctionParams2.getAlpha2(): double; begin result := _alpha2; end;
  function TFrameFunctionParams2.getBeta(): double; begin result := _beta; end;
  function TFrameFunctionParams2.getLocation(): double; begin result := _location; end;
  function TFrameFunctionParams2.getScale(): double; begin result := _scale; end;
  function TFrameFunctionParams2.getShape(): double; begin result := _shape; end;
  function TFrameFunctionParams2.getZeta(): double; begin result := _zeta; end;
  function TFrameFunctionParams2.getSigma(): double; begin result := _sigma; end;

  function TFrameFunctionParams2.getUnitsLocked(): boolean; begin result := _unitsLocked; end;
  //procedure TFrameFunctionParams2.setUnitsLocked( val: boolean ); begin _unitsLocked := val; end;

  function TFrameFunctionParams2.getNameLocked(): boolean; begin result := _nameLocked; end;
  procedure TFrameFunctionParams2.setNameLocked( val: boolean ); begin _nameLocked := val; end;

  function TFrameFunctionParams2.getNameVis(): boolean; begin result := _nameVis; end;
  procedure TFrameFunctionParams2.setNameVis( val: boolean ); begin _nameVis := val; end;

  function TFrameFunctionParams2.getEditEnabled(): boolean; begin result := _editEnabled; end;

  // This function deals with a little bug in TREEdit.
  procedure TFrameFunctionParams2.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    var
      rle: TREEdit;
    begin
      rle := sender as TREEdit;
      if( rle.SelLength = length( rle.Text ) ) then rle.Text := '';
    end
  ;
	
	
	
	
	
	
	
	
	
	
	
	
	
end.
