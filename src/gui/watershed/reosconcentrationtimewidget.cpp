/***************************************************************************
  reosconcentrationtimewidget.cpp - ReosConcentrationTimeWidget

 ---------------------
 begin                : 14.2.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosconcentrationtimewidget.h"
#include "ui_reosconcentrationtimewidget.h"

#include <QDialog>
#include <QToolBar>
#include <QLayout>
#include <QClipboard>

#include "reosparameterwidget.h"
#include "reoswatershed.h"
#include "reosconcentrationtimecalculation.h"

ReosConcentrationTimeWidget::ReosConcentrationTimeWidget( QWidget *parent ) :
  ReosActionWidget( parent )
  , ui( new Ui::ReosConcentrationTimeWidget )
  , mFormulasModel( new ReosConcentrationTimeFormulasModel( ReosConcentrationTimeFormulasRegistery::instance(), this ) )
  , mLengthParameterWidget( new ReosParameterDoubleWidget( this, tr( "Longest path" ) ) )
  , mDropParameterWidget( new ReosParameterDoubleWidget( this, tr( "Drop" ) ) )
  , mSlopeParameterWidget( new ReosParameterSlopeWidget( this, tr( "Slope" ) ) )
  , mAreaParameterWidget( new ReosParameterAreaWidget( this, tr( "Area" ) ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  ui->usedValue->setDefaultName( tr( "Used value" ) );
  ui->usedValue->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->usedValue->hideWhenVoid( false );

  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaKirpich );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaPassini );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaVentura );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaJohnstone );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaVenTeChow );

  QToolBar *toolBar = new QToolBar( ui->widgetTools );
  ui->widgetTools->layout()->addWidget( toolBar );

  toolBar->addAction( QPixmap( QStringLiteral( ":/images/selectAllFormula.svg" ) ), tr( "Select All " ), this, &ReosConcentrationTimeWidget::onSelectAll );
  toolBar->addAction( QPixmap( QStringLiteral( ":/images/deselectAllFormula.svg" ) ), tr( "Deselect All " ), this, &ReosConcentrationTimeWidget::onDeselectAll );
  toolBar->addAction( QPixmap( QStringLiteral( ":/images/selectValidFormula.svg" ) ), tr( "Select Valid " ), this, &ReosConcentrationTimeWidget::onSelectValid );
  toolBar->addAction( QPixmap( QStringLiteral( ":/images/copySelected.svg" ) ), tr( "Copy values to Clipboard " ), this, &ReosConcentrationTimeWidget::onCopy );

  ui->formulasView->setModel( mFormulasModel );
  ui->groupBoxparameters->layout()->addWidget( mLengthParameterWidget );
  mLengthParameterWidget->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  mLengthParameterWidget->hideWhenVoid( false );
  ui->groupBoxparameters->layout()->addWidget( mDropParameterWidget );
  mDropParameterWidget->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  mDropParameterWidget->hideWhenVoid( false );
  ui->groupBoxparameters->layout()->addWidget( mSlopeParameterWidget );
  mSlopeParameterWidget->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  mSlopeParameterWidget->hideWhenVoid( false );
  ui->groupBoxparameters->layout()->addWidget( mAreaParameterWidget );
  mAreaParameterWidget->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  mAreaParameterWidget->hideWhenVoid( false );

  connect( mAreaParameterWidget, &ReosParameterWidget::valueChanged, this, &ReosConcentrationTimeWidget::updateFormulas );
  connect( mLengthParameterWidget, &ReosParameterWidget::valueChanged, this, &ReosConcentrationTimeWidget::updateFormulas );
  connect( mDropParameterWidget, &ReosParameterWidget::valueChanged, this, &ReosConcentrationTimeWidget::updateFormulas );
  connect( mSlopeParameterWidget, &ReosParameterWidget::valueChanged, this, &ReosConcentrationTimeWidget::updateFormulas );
  connect( ui->usedValue, &ReosParameterWidget::valueChanged, this, &ReosConcentrationTimeWidget::updateFormulas );

  connect( ui->radioButtonAverage, &QRadioButton::clicked, this, &ReosConcentrationTimeWidget::onMethodChanged );
  connect( ui->radioButtonMaximum, &QRadioButton::clicked, this, &ReosConcentrationTimeWidget::onMethodChanged );
  connect( ui->radioButtonMinimum, &QRadioButton::clicked, this, &ReosConcentrationTimeWidget::onMethodChanged );
  connect( ui->radioButtonChoosen, &QRadioButton::clicked, this, &ReosConcentrationTimeWidget::onMethodChanged );

  connect( ui->formulasView, &QAbstractItemView::doubleClicked, this, &ReosConcentrationTimeWidget::onViewDoubleClicked );

  connect( mFormulasModel, &ReosConcentrationTimeFormulasModel::activeFormulasChanged, this, &ReosConcentrationTimeWidget::applyCalculation );

  connect( ui->usedValue, &ReosParameterWidget::unitChanged, mFormulasModel, [this]()
  {
    ReosParameterDuration *durationParameter = this->ui->usedValue->durationParameter();
    if ( durationParameter )
      this->mFormulasModel->setCurrentTimeUnit( durationParameter->value().unit() );
  } );

  connect( ui->toolButtonForumulaDisplaying, &QToolButton::clicked, this, &ReosConcentrationTimeWidget::onFormulaDisplaying );

}

ReosConcentrationTimeWidget::~ReosConcentrationTimeWidget()
{
  delete ui;
}

void ReosConcentrationTimeWidget::setCurrentWatershed( ReosWatershed *ws )
{
  mCurrentWatershed = ws;

  if ( !mCurrentWatershed )
  {
    mLengthParameterWidget->setDouble( nullptr );
    mDropParameterWidget->setDouble( nullptr );
    mSlopeParameterWidget->setSlope( nullptr );
    mAreaParameterWidget->setArea( nullptr );
    ui->usedValue->setDuration( nullptr );
    setUsedMethod( ReosConcentrationTimeCalculation::Average );
    mFormulasModel->setActiveFormulas( QStringList() );
    mFormulasModel->setChoosenFormula( QString() );
  }
  else
  {
    mLengthParameterWidget->setDouble( ws->longestPath() );
    mDropParameterWidget->setDouble( ws->drop() );
    mSlopeParameterWidget->setSlope( ws->slope() );
    mAreaParameterWidget->setArea( ws->area() );
    ui->usedValue->setDuration( ws->concentrationTime() );
    setUsedMethod( mCurrentWatershed->concentrationTimeCalculation().usedMethod() );
    mFormulasModel->setActiveFormulas( mCurrentWatershed->concentrationTimeCalculation().activeFormulas() );
    mFormulasModel->setChoosenFormula( mCurrentWatershed->concentrationTimeCalculation().userChoosenFormula() );
    mFormulasModel->setCurrentTimeUnit( ws->concentrationTime()->value().unit() );
  }

  mFormulasModel->highlightChoosenFormula( usedMethod() == ReosConcentrationTimeCalculation::UserChoosenFormula );

  updateFormulas();
}

void ReosConcentrationTimeWidget::updateFormulas()
{
  ReosConcentrationTimeFormula::Parameters params;
  if ( mAreaParameterWidget->areaParameter() )
    params.area = mAreaParameterWidget->areaParameter()->value();
  if ( mDropParameterWidget->doubleParameter() )
    params.drop = mDropParameterWidget->doubleParameter()->value();
  if ( mLengthParameterWidget->doubleParameter() )
    params.length = mLengthParameterWidget->doubleParameter()->value();
  if ( mSlopeParameterWidget->slopeParameter() )
    params.slope = mSlopeParameterWidget->slopeParameter()->value();

  mFormulasModel->setParameters( params );

  if ( mCurrentWatershed )
    mFormulasModel->setActiveFormulas( mCurrentWatershed->concentrationTimeCalculation().activeFormulas() );
}

void ReosConcentrationTimeWidget::applyCalculation()
{
  if ( !mCurrentWatershed )
    return;

  ReosConcentrationTimeCalculation calculation = mCurrentWatershed->concentrationTimeCalculation();
  calculation.setActiveFormula( mFormulasModel->activeFormulas() );
  calculation.setUserChoosenFormula( mFormulasModel->choosenFormula() );
  calculation.setUsedMethod( usedMethod() );

  mCurrentWatershed->setConcentrationTimeCalculation( calculation );

  updateCalcultedValue();
}

void ReosConcentrationTimeWidget::onMethodChanged()
{
  if ( !mCurrentWatershed )
    return;

  if ( mCurrentWatershed->concentrationTimeCalculation().usedMethod() == usedMethod() )
    return;

  applyCalculation();
  mFormulasModel->highlightChoosenFormula( usedMethod() == ReosConcentrationTimeCalculation::UserChoosenFormula );
}

void ReosConcentrationTimeWidget::updateCalcultedValue()
{
  ui->usedValue->durationParameter()->updateIfNecessary();
}

void ReosConcentrationTimeWidget::onSelectAll()
{
  QStringList allFormulas = ReosConcentrationTimeFormulasRegistery::instance()->formulasList();
  mFormulasModel->setActiveFormulas( allFormulas );
  applyCalculation();
}

void ReosConcentrationTimeWidget::onDeselectAll()
{
  mFormulasModel->setActiveFormulas( QStringList() );
  applyCalculation();
}

void ReosConcentrationTimeWidget::onSelectValid()
{
  QStringList allFormulas = ReosConcentrationTimeFormulasRegistery::instance()->formulasList();
  QStringList selectedFormulas;
  for ( const QString &f : allFormulas )
  {
    if ( ReosConcentrationTimeFormulasRegistery::instance()->formula( f )->isInValidityDomain( mFormulasModel->parameters() ) )
      selectedFormulas.append( f );
  }

  mFormulasModel->setActiveFormulas( selectedFormulas );
  applyCalculation();
}


void ReosConcentrationTimeWidget::onCopy()
{

  QClipboard *clipBoard = QApplication::clipboard();
  QString copyText = mFormulasModel->textData();

  if ( ui->usedValue->durationParameter() )
  {
    switch ( usedMethod() )
    {
      case ReosConcentrationTimeCalculation::Maximum:
        copyText.append( tr( "Maximum value" ) );
        break;
      case ReosConcentrationTimeCalculation::Minimum:
        copyText.append( tr( "Minimum value" ) );
        break;
      case ReosConcentrationTimeCalculation::Average:
        copyText.append( tr( "Average value" ) );
        break;
      case ReosConcentrationTimeCalculation::UserChoosenFormula:
        copyText.append( tr( "Choosen formula (%1)" ).arg( mFormulasModel->choosenFormula() ) );
        break;
    }

    copyText.append( QStringLiteral( "\t" ) );
    copyText.append( ui->usedValue->durationParameter()->toString( 2 ) );
    copyText.append( QStringLiteral( "\n" ) );
  }

  clipBoard->setText( copyText );
}

void ReosConcentrationTimeWidget::onViewDoubleClicked( const QModelIndex &index )
{
  if ( !ui->radioButtonChoosen->isChecked() )
    return;

  mFormulasModel->setChoosenFormula( index );
  applyCalculation();
}

void ReosConcentrationTimeWidget::onFormulaDisplaying()
{
  QDialog *dialog = new QDialog( this );

  QVBoxLayout *vertLayout = new QVBoxLayout;
  dialog->setLayout( vertLayout );

  auto addLine = [this, vertLayout]
  {
    QFrame *line = new QFrame( this );
    line->setFrameShape( QFrame::HLine );
    line->setFrameShadow( QFrame::Sunken );
    vertLayout->addWidget( line );
  };



  dialog->layout()->addWidget( new QLabel( tr( "Concentration Time Formula" ), this ) );
  addLine();

  if ( ReosConcentrationTimeFormulasRegistery::isInstantiate() )
  {
    ReosConcentrationTimeFormulasRegistery *registery = ReosConcentrationTimeFormulasRegistery::instance();

    for ( const QString &formulaName : registery->formulasList() )
    {
      QHBoxLayout *lay = new QHBoxLayout( this );
      vertLayout->addLayout( lay );
      lay->addWidget( new QLabel( formulaName, this ) );
      QLabel *formulaLabel = new QLabel( this );
      formulaLabel->setPixmap( registery->formula( formulaName )->formulaImage() );
      lay->addWidget( formulaLabel );
      addLine();

    }
  }

  dialog->layout()->addWidget( new QLabel( tr( "L : longest path (m)" ), this ) );
  dialog->layout()->addWidget( new QLabel( tr( "A : watershed area (m²)" ), this ) );
  dialog->layout()->addWidget( new QLabel( tr( "i : average slope (m/m)" ), this ) );
  dialog->layout()->addWidget( new QLabel( tr( "H : Drop (m)" ), this ) );
  dialog->layout()->addWidget( new QLabel( tr( "All results in minutes" ), this ) );

  dialog->exec();
}

ReosConcentrationTimeCalculation::UsedMethod ReosConcentrationTimeWidget::usedMethod() const
{
  if ( ui->radioButtonAverage->isChecked() )
    return ReosConcentrationTimeCalculation::Average;

  if ( ui->radioButtonChoosen->isChecked() )
    return ReosConcentrationTimeCalculation::UserChoosenFormula;

  if ( ui->radioButtonMaximum->isChecked() )
    return ReosConcentrationTimeCalculation::Maximum;

  if ( ui->radioButtonMinimum->isChecked() )
    return ReosConcentrationTimeCalculation::Minimum;

  return ReosConcentrationTimeCalculation::Average;
}

void ReosConcentrationTimeWidget::setUsedMethod( ReosConcentrationTimeCalculation::UsedMethod method )
{
  if ( method == ReosConcentrationTimeCalculation::Average )
    ui->radioButtonAverage->setChecked( true );

  if ( method == ReosConcentrationTimeCalculation::UserChoosenFormula )
    ui->radioButtonChoosen->setChecked( true );

  if ( method == ReosConcentrationTimeCalculation::Minimum )
    ui->radioButtonMinimum->setChecked( true );

  if ( method == ReosConcentrationTimeCalculation::Maximum )
    ui->radioButtonMaximum->setChecked( true );
}
