#include "reosaddhydrographnodefromwidget.h"
#include "ui_reosaddhydrographnodefromwidget.h"

#include <QMessageBox>

#include "reosguicontext.h"
#include "reosdataprovider.h"
#include "reosdataprovidergui.h"
#include "reoshydrograph.h"
#include "reossettings.h"
#include "reosmapextent.h"
#include "reoshydrographsource.h"

ReosAddHydrographNodeFromWidget::ReosAddHydrographNodeFromWidget( ReosHydraulicNetwork *network, const ReosGuiContext &context )
  : ReosActionWidget( context.parent() )
  , ui( new Ui::ReosAddHydrographNodeFromWidget )
  , mMap( context.map() )
  , mNetWork( network )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );
  const QStringList providerKeys = ReosDataProviderRegistery::instance()->withCapabilities( ReosHydrograph::staticType(), ReosDataProvider::Spatial );

  for ( const QString &key : providerKeys )
    ui->mProviderComboBox->addItem( ReosDataProviderGuiRegistery::instance()->providerIcon( key ),
                                    ReosDataProviderGuiRegistery::instance()->providerDisplayText( key ),
                                    key );

  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "/add-hydrograph-from/provider" ) ) )
  {
    int index = ui->mProviderComboBox->findData( settings.value( "/add-hydrograph-from/provider" ) );
    if ( index >= 0 )
      ui->mProviderComboBox->setCurrentIndex( index );
  }

  connect( ui->mProviderComboBox,  QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosAddHydrographNodeFromWidget::onProviderChanged );

  connect( ui->mCloseButton, &QPushButton::clicked, this, &ReosActionWidget::close );
  connect( ui->mAddNode, &QPushButton::clicked, this, &ReosAddHydrographNodeFromWidget::onAddNode );
  connect( ui->mAddNodeCopy, &QPushButton::clicked, this, &ReosAddHydrographNodeFromWidget::onAddNodeCopy );

  connect( this, &ReosActionWidget::opened, this, &ReosAddHydrographNodeFromWidget::onProviderChanged );
  connect( this, &ReosActionWidget::closed, this, [this]
  {
    mCurrentWidget->deleteLater();
    mCurrentWidget = nullptr;
  } );
}

ReosAddHydrographNodeFromWidget::~ReosAddHydrographNodeFromWidget()
{
  delete ui;
}

void ReosAddHydrographNodeFromWidget::onProviderChanged()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/add-hydrograph-from/provider" ), ui->mProviderComboBox->currentData() );
  if ( mCurrentWidget )
  {
    ui->mProviderLayout->removeWidget( mCurrentWidget );
    delete mCurrentWidget;
  }
  const QString providerKey = ui->mProviderComboBox->currentData().toString();
  mCurrentWidget = ReosDataProviderGuiRegistery::instance()->createProviderSelectorWidget( providerKey, ReosHydrograph::staticType(), mMap, this );

  mIsDatasetSelected = false;
  mIsDataReady = false;
  ui->mAddNode->setEnabled( false );
  ui->mAddNodeCopy->setEnabled( false );
  if ( mCurrentWidget )
  {
    ui->mProviderLayout->addWidget( mCurrentWidget );

    connect( mCurrentWidget, &ReosDataProviderSelectorWidget::dataSelectionChanged, this, [this ]( bool isDataSelected )
    {
      mIsDatasetSelected = isDataSelected;
      ui->mAddNode->setEnabled( isDataSelected );
      if ( !isDataSelected )
      {
        mIsDataReady = false;
        ui->mAddNode->setEnabled( false );
        ui->mAddNodeCopy->setEnabled( false );
      }
    } );

    // connect loading
    connect( mCurrentWidget, &ReosDataProviderSelectorWidget::dataIsLoading, this, [this]
    {
      mIsDataReady = false;
      ui->mAddNodeCopy->setEnabled( false );
    } );

    // connect data is ready
    connect( mCurrentWidget, &ReosDataProviderSelectorWidget::dataIsReady, this, [this]
    {
      mIsDataReady = true;
      ui->mAddNodeCopy->setEnabled( true );
    } );

    mCurrentWidget->onOpened();
  }

}

void ReosAddHydrographNodeFromWidget::onAddNodeCopy()
{
  if ( ! mCurrentWidget )
    return;
  std::unique_ptr<ReosHydrograph> copyHyd = std::make_unique<ReosHydrograph>();
  ReosHydrograph *providerHydrograph = qobject_cast<ReosHydrograph *>( mCurrentWidget->selectedData() );
  if ( providerHydrograph )
  {
    copyHyd->setName( providerHydrograph->name() );
    copyHyd->copyFrom( providerHydrograph );
    copyHyd->setColor( providerHydrograph->color() );
  }
  else
  {
    return;
  }

  QVariantMap meta = mCurrentWidget->selectedMetadata();
  addNode( meta, copyHyd.release() );
}

void ReosAddHydrographNodeFromWidget::onAddNode()
{
  if ( ! mCurrentWidget )
    return;

  std::unique_ptr<ReosHydrograph> hyd;
  hyd.reset( qobject_cast<ReosHydrograph *>( mCurrentWidget->createData() ) );
  if ( !hyd )
    return;
  QVariantMap meta = mCurrentWidget->selectedMetadata();
  addNode( meta, hyd.release() );
}

void ReosAddHydrographNodeFromWidget::addNode( const QVariantMap &metadata, ReosHydrograph *hydrograph )
{
  bool ok;
  std::unique_ptr<ReosHydrograph> hyd;
  hyd.reset( hydrograph );
  double xCoord = metadata.value( QStringLiteral( "x-coord" ) ).toDouble( &ok );
  if ( ok )
  {
    double yCoord = metadata.value( QStringLiteral( "y-coord" ) ).toDouble( &ok );
    if ( ok )
    {
      QString crs = metadata.value( QStringLiteral( "crs" ) ).toString();
      ReosSpatialPosition position( xCoord, yCoord, crs );

      std::unique_ptr<ReosHydrographJunction> junctionNode =
        std::make_unique<ReosHydrographJunction>( position, mNetWork );

      int hydIndex = junctionNode->gaugedHydrographsStore()->hydrographCount();
      junctionNode->elementName()->setValue( metadata.value( QStringLiteral( "station" ) ).toString() );
      junctionNode->setInternalHydrographOrigin( ReosHydrographJunction::GaugedHydrograph );
      junctionNode->gaugedHydrographsStore()->addHydrograph( hyd.release() );
      junctionNode->setGaugedHydrographIndex( hydIndex );
      mNetWork->addElement( junctionNode.release(), true );

      close();
      return;
    }
  }

  QMessageBox::warning( this, tr( "Unable to Add Node" ),
                        tr( "Position associated with this hydrograh is invalid.\n"
                            "Unable to add a node." ) );
}
