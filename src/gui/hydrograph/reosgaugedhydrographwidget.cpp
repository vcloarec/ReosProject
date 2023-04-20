/***************************************************************************
  reosgaugedhydrographwidget.cpp - ReosGaugedHydrographWidget

 ---------------------
 begin                : 24.10.2021
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
#include "reosgaugedhydrographwidget.h"
#include "ui_reosgaugedhydrographwidget.h"

#include <QMessageBox>
#include <QPushButton>

#include "reosmap.h"
#include "reoswatershed.h"
#include "reoshydrograph.h"
#include "reosformwidget.h"
#include "reoshydrographeditingwidget.h"
#include "reosplottimeconstantinterval.h"
#include "reosdataprovidergui.h"
#include "reosstyleregistery.h"

ReosGaugedHydrographWidget::ReosGaugedHydrographWidget( const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosGaugedHydrographWidget )
  , mMap( guiContext.map() )
{
  ui->setupUi( this );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosHydrographEditingWidgetFactory );

  ui->mWidgetToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->mWidgetToolBar );
  toolBar->setIconSize( QSize( 16, 16 ) );
  toolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->mWidgetToolBar->layout()->addWidget( toolBar );

  mActionAddHydrograph = toolBar->addAction( QIcon( QStringLiteral( ":/images/add.svg" ) ), tr( "Add Gauged Hydrograph" ), this, &ReosGaugedHydrographWidget::onAddHydrograph );
  mActionDeleteHydrograph = toolBar->addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Delete Current Hydrograph" ), this, &ReosGaugedHydrographWidget::onRemoveHydrograph );
  mActionRenameHydrograph = toolBar->addAction( QIcon( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Current Hydrograph" ), this, &ReosGaugedHydrographWidget::onRenameHydrograph );

  ui->mWidgetProviderToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetProviderToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  mToolBarProvider = new QToolBar( ui->mWidgetProviderToolBar );
  mToolBarProvider->setIconSize( QSize( 24, 24 ) );
  mToolBarProvider->layout()->setContentsMargins( 0, 0, 0, 0 );
  mToolBarProvider->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->mWidgetProviderToolBar->layout()->addWidget( mToolBarProvider );

  mHydrographPlot = new ReosPlotTimeSerieVariableStep( tr( "Hydrograph" ) );
  ui->plotWidget->setSettingsContext( QStringLiteral( "gauged-hydrograph" ) );
  ui->plotWidget->addPlotItem( mHydrographPlot );
  ui->plotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->plotWidget->enableAutoMinimumSize( true );
  ui->plotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
  ui->plotWidget->setLegendEnabled( false );
  ui->plotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );

  populateProviderActions();

  connect( ui->mComboBoxHydrographName, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGaugedHydrographWidget::onCurrentHydrographChanged );

  ui->mBackButton->setVisible( false );
  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  onStoreChanged();
}

ReosGaugedHydrographWidget::~ReosGaugedHydrographWidget()
{
  delete ui;
}

void ReosGaugedHydrographWidget::setHydrographStore( ReosHydrographsStore *store )
{
  if ( mHydrographStore )
    disconnect( mHydrographStore, &ReosDataObject::dataChanged, this, &ReosGaugedHydrographWidget::onStoreChanged );

  mHydrographStore = store;
  if ( !mHydrographStore )
  {
    mHydrographPlot->setTimeSeries( nullptr );
    setEnabled( false );
  }
  else
  {
    setEnabled( true );
    connect( mHydrographStore, &ReosDataObject::dataChanged, this, &ReosGaugedHydrographWidget::onStoreChanged );
  }

  onStoreChanged();
  onCurrentHydrographChanged();
}

void ReosGaugedHydrographWidget::showBackButton()
{
  ui->mBackButton->setVisible( true );
}

void ReosGaugedHydrographWidget::onAddHydrograph()
{
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Add Hydrograph" ) );
  dial->addText( tr( "Add a new hydrograph, choose a name:" ) );
  ReosParameterString nameParam( tr( "Hydrograph name" ) );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    std::unique_ptr<ReosHydrograph> newHydrograph = std::make_unique<ReosHydrograph>();
    newHydrograph->setName( nameParam.value() );
    newHydrograph->setColor( Qt::blue );
    newHydrograph->setReferenceTime( QDateTime( QDate( QDate::currentDate().year(), 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
    mHydrographStore->addHydrograph( newHydrograph.release() );
    onStoreChanged();
    ui->mComboBoxHydrographName->setCurrentIndex( ui->mComboBoxHydrographName->count() - 1 );
  }

  dial->deleteLater();
}

void ReosGaugedHydrographWidget::onRemoveHydrograph()
{
  if ( !mHydrographStore )
    return;

  int currentIndex = ui->mComboBoxHydrographName->currentIndex();
  ReosHydrograph *hydrographToReMove = mHydrographStore->hydrograph( currentIndex );
  if ( !hydrographToReMove )
    return;

  if ( QMessageBox::warning( this, tr( "Remove Gauged Hydrograph" ),
                             tr( "Do you want to remove the hydrograph '%1'?" ).arg( hydrographToReMove->name() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
  {
    mHydrographStore->removeHydrograph( currentIndex );
    ui->mComboBoxHydrographName->removeItem( currentIndex );
  }

  onStoreChanged();
}

void ReosGaugedHydrographWidget::onRenameHydrograph()
{
  if ( !mHydrographStore )
    return;

  int currentIndex = ui->mComboBoxHydrographName->currentIndex();
  ReosHydrograph *hydrographToRename = mHydrographStore->hydrograph( currentIndex );
  if ( !hydrographToRename )
    return;

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Rename Gauged Hydrograph" ) );
  dial->addText( tr( "Rename gauged hydrograph'%1',\nchoose a new name:" ).arg( hydrographToRename->name() ) );
  ReosParameterString nameParam( tr( "Hydrograph name" ) );
  nameParam.setValue( hydrographToRename->name() );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    hydrographToRename->setName( nameParam.value() );
    ui->mComboBoxHydrographName->setItemText( currentIndex, nameParam.value() );
  }

  dial->deleteLater();
}

void ReosGaugedHydrographWidget::onStoreChanged()
{
  ui->mComboBoxHydrographName->clear();
  bool hasHydropraph = false;

  if ( mHydrographStore )
  {
    const QStringList &names = mHydrographStore->hydrographNames();
    ui->mComboBoxHydrographName->addItems( names );
    hasHydropraph = !names.isEmpty();
    mActionAddHydrograph->setEnabled( true );
  }
  else
  {
    mActionAddHydrograph->setEnabled( false );
  }

  mActionDeleteHydrograph->setEnabled( hasHydropraph );
  mActionRenameHydrograph->setEnabled( hasHydropraph );
}

void ReosGaugedHydrographWidget::onCurrentHydrographChanged()
{
  if ( !mCurrentHydrograph.isNull() )
  {
    disconnect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosGaugedHydrographWidget::updatePlotExtent );
  }

  if ( mHydrographStore && mHydrographStore->hydrographCount() > 0 )
    mCurrentHydrograph = mHydrographStore->hydrograph( ui->mComboBoxHydrographName->currentIndex() );
  else
    mCurrentHydrograph = nullptr;

  std::unique_ptr<QWidget> newEditingWidget;
  std::unique_ptr<QWidget> newSettingsProviderWidget;

  if ( !mCurrentHydrograph.isNull() )
  {
    newSettingsProviderWidget.reset(
      ReosDataProviderGuiRegistery::instance()->createProviderSettingsWidget( mCurrentHydrograph->dataProvider() ) );
    newEditingWidget.reset( ReosFormWidgetFactories::instance()->createDataFormWidget( mCurrentHydrograph ) );
    connect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosGaugedHydrographWidget::updatePlotExtent );
  }
  else
  {
    newEditingWidget.reset( new QLabel( tr( "No Hydrograph" ) ) );
  }

  mHydrographPlot->setTimeSeries( mCurrentHydrograph );
  ui->plotWidget->updatePlot();

  if ( ui->mEditingWidgetLayout->count() != 0 )
  {
    ui->mEditingWidgetLayout->removeWidget( mCurrenEditingWidget );
    delete mCurrenEditingWidget;
    ui->mEditingWidgetLayout->removeWidget( mCurrentProviderSettingsWidget );
    delete mCurrentProviderSettingsWidget;
  }

  mCurrentProviderSettingsWidget = newSettingsProviderWidget.release();
  if ( mCurrentProviderSettingsWidget )
    ui->mEditingWidgetLayout->addWidget( mCurrentProviderSettingsWidget );

  mCurrenEditingWidget = newEditingWidget.release();
  ui->mEditingWidgetLayout->addWidget( mCurrenEditingWidget );

  ui->mEditingWidgetLayout->setStretch( ui->mEditingWidgetLayout->count() - 1, 1 );
}

void ReosGaugedHydrographWidget::updatePlotExtent()
{
  if ( mCurrentHydrograph.isNull() )
    return;

  const QPair<QDateTime, QDateTime> timeExtent = mCurrentHydrograph->timeExtent();

  ui->plotWidget->setAxeXExtent( timeExtent.first, timeExtent.second );
}

void ReosGaugedHydrographWidget::populateProviderActions()
{
  const QString dataType = ReosHydrograph::staticType();

  const QStringList providers =
    ReosDataProviderGuiRegistery::instance()->providers( dataType, ReosDataProviderGuiFactory::GuiCapability::DataSelector );

  for ( const QString &provider : providers )
  {
    QAction *action = new QAction(
      ReosDataProviderGuiRegistery::instance()->providerIcon( provider ),
      ReosDataProviderGuiRegistery::instance()->providerDisplayText( provider ), this );
    mProvidersActionToKeys.insert( action, provider );
    mToolBarProvider->addAction( action );

    connect( action, &QAction::triggered, this, [this, provider]
    {
      showProviderSelector( provider );
    } );
  }

}

void ReosGaugedHydrographWidget::showProviderSelector( const QString &providerKey )
{
  const QString dataType = ReosHydrograph::staticType();

  std::unique_ptr<ReosStackedPageWidget> providerPage = std::make_unique<ReosStackedPageWidget>();
  QVBoxLayout *mainLayout = new QVBoxLayout;
  providerPage->setLayout( mainLayout );
  mainLayout->setContentsMargins( 0, 0, 0, 0 );
  QHBoxLayout *buttonLayout = new QHBoxLayout;
  buttonLayout->setContentsMargins( 0, 0, 0, 0 );
  buttonLayout->setSpacing( 6 );
  QPushButton *buttonBack = new QPushButton( tr( "Back" ), providerPage.get() );
  buttonLayout->addWidget( buttonBack );
  QSpacerItem *headerSpacer = new QSpacerItem( 40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum );
  buttonLayout->addSpacerItem( headerSpacer );

  headerSpacer->changeSize( headerSpacer->geometry().width(),
                            ui->mainHeaderLayout->sizeHint().height(),
                            QSizePolicy::Expanding );

  QPushButton *addButton = new QPushButton( tr( "Add" ), providerPage.get() );
  QPushButton *addCopyButton = new QPushButton( tr( "Add Copy" ), providerPage.get() );
  buttonLayout->addWidget( addButton );
  buttonLayout->addWidget( addCopyButton );
  addButton->setEnabled( false );
  addCopyButton->setEnabled( false );

  QFrame *line = new QFrame( providerPage.get() );
  line->setObjectName( QString::fromUtf8( "line" ) );
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );

  mainLayout->addItem( buttonLayout );
  mainLayout->addWidget( line );

  ReosDataProviderSelectorWidget *currentDataSelectorWidget =
    ReosDataProviderGuiRegistery::instance()->createProviderSelectorWidget( providerKey, dataType, mMap, providerPage.get() );

  mainLayout->addWidget( currentDataSelectorWidget );

  mainLayout->setStretch( 2, 1 );

  connect( buttonBack, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  connect( addButton, &QPushButton::clicked, this, [this, currentDataSelectorWidget]
  {
    if ( !currentDataSelectorWidget )
      return;
    std::unique_ptr<ReosHydrograph> hyd;
    hyd.reset( qobject_cast<ReosHydrograph *>( currentDataSelectorWidget->createData() ) );
    if ( !hyd )
      return;
    mHydrographStore->addHydrograph( hyd.release() );
    onStoreChanged();
    emit backToPreviousPage();
    ui->mComboBoxHydrographName->setCurrentIndex( ui->mComboBoxHydrographName->count() - 1 );
  } );

  connect( addCopyButton, &QPushButton::clicked, this, [this, currentDataSelectorWidget]
  {
    if ( !currentDataSelectorWidget )
      return;
    std::unique_ptr<ReosHydrograph> copyHyd = std::make_unique<ReosHydrograph>();

    ReosHydrograph *providerHydrograph = qobject_cast<ReosHydrograph *>( currentDataSelectorWidget->selectedData() );

    if ( providerHydrograph )
    {
      copyHyd->setName( providerHydrograph->name() );
      copyHyd->copyFrom( providerHydrograph );
      copyHyd->setColor( providerHydrograph->color() );
      mHydrographStore->addHydrograph( copyHyd.release() );
      onStoreChanged();
      ui->mComboBoxHydrographName->setCurrentIndex( ui->mComboBoxHydrographName->count() - 1 );
    }

    emit backToPreviousPage();
  } );

  if ( currentDataSelectorWidget )
  {
    connect( currentDataSelectorWidget, &ReosDataProviderSelectorWidget::dataSelectionChanged, this, [this, addButton, addCopyButton]( bool isDataSelected )
    {
      mIsDatasetSelected = isDataSelected;
      addButton->setEnabled( isDataSelected  && mHydrographStore );
      if ( !isDataSelected )
      {
        mIsDataReady = false;
        addButton->setEnabled( false );
        addCopyButton->setEnabled( false );
      }
    } );

    // connect loading
    connect( currentDataSelectorWidget, &ReosDataProviderSelectorWidget::dataIsLoading, this, [this, addCopyButton]
    {
      mIsDataReady = false;
      addCopyButton->setEnabled( false );
    } );

    // connect data is ready
    connect( currentDataSelectorWidget, &ReosDataProviderSelectorWidget::dataIsReady, this, [this, addCopyButton]
    {
      mIsDataReady = true;
      addCopyButton->setEnabled( mHydrographStore != nullptr );
    } );

    currentDataSelectorWidget->onOpened();
  }

  addOtherPage( providerPage.release(), true );
}

ReosWatershedGaugedHydrographWidget::ReosWatershedGaugedHydrographWidget( const ReosGuiContext &guiContext )
  : ReosActionStackedWidget( guiContext.parent() )
  , mGaugedHydrographWidget( new ReosGaugedHydrographWidget( guiContext ) )
{
  setWindowFlag( Qt::Dialog );
  setWindowTitle( tr( "Watershed Gauged Hydrograph" ) );
  addPage( mGaugedHydrographWidget, 0, false );
}

void ReosWatershedGaugedHydrographWidget::setCurrentWatershed( ReosWatershed *watershed )
{
  mGaugedHydrographWidget->setEnabled( watershed != nullptr );

  if ( watershed )
    mGaugedHydrographWidget->setHydrographStore( watershed->gaugedHydrographs() );
  else
    mGaugedHydrographWidget->setHydrographStore( nullptr );
}
