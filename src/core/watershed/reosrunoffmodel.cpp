/***************************************************************************
  reosrunoffmodel.cpp - ReosRunoffModel

 ---------------------
 begin                : 17.2.2021
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
#include "reosrunoffmodel.h"

#include <QFile>
#include <QFileInfo>

#include "reosrainfallitem.h"
#include "reostimeserie.h"
#include "reosparameter.h"
#include "reosversion.h"

#define  FILE_MAGIC_NUMBER 1909201402

ReosRunoffModel::ReosRunoffModel( const QString &name, QObject *parent ):
  ReosDataObject( parent )
  , mName( new ReosParameterString( QObject::tr( "Name" ), false, this ) )
{
  mName->setValue( name );
  mUniqueId = QUuid::createUuid().toString();
}

ReosRunoffModel::ReosRunoffModel( const ReosEncodedElement &element, QObject *parent ):
  ReosDataObject( parent )
{
  mName = ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false,  QObject::tr( "Name" ), this );
  element.getData( QStringLiteral( "unique-id" ), mUniqueId );
  if ( mUniqueId.isEmpty() )
    mUniqueId = QUuid::createUuid().toString();
}

ReosParameterString *ReosRunoffModel::name() const
{
  return mName;
}

QString ReosRunoffModel::uniqueId() const
{
  return mUniqueId;
}

void ReosRunoffModel::connectParameters()
{
  QList<ReosParameter *> params = parameters();
  for ( int i = 1; i < params.count(); ++i ) //do not take the first one that should be the name
    connect( params.at( i ), &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

void ReosRunoffModel::encodeBase( ReosEncodedElement &element ) const
{
  element.addEncodedData( QStringLiteral( "name" ), mName->encode() );
  element.addData( QStringLiteral( "unique-id" ), mUniqueId );
}

ReosRunoff::ReosRunoff( ReosRunoffModelsGroup *runoffModels, ReosTimeSerieConstantInterval *rainfall, QObject *parent ):
  ReosDataObject( parent )
  , mRainfall( rainfall )
  , mRunoffModelsGroups( runoffModels )
  , mData( new ReosTimeSerieConstantInterval( this ) )
{
  mData->copyAttribute( mRainfall );
  mData->syncWith( mRainfall );

  registerUpstreamData( mRainfall );
  registerUpstreamData( mRunoffModelsGroups );
}

void ReosRunoff::setRainfall( ReosTimeSerieConstantInterval *rainfall )
{
  deregisterUpstreamData( mRainfall );
  mRainfall = rainfall;
  setObsolete();
  registerUpstreamData( mRainfall );
}

int ReosRunoff::valueCount() const
{
  updateValues();
  return mData->valueCount();
}

ReosDuration ReosRunoff::timeStep() const
{
  updateValues();
  return mRainfall->timeStepParameter()->value();
}

double ReosRunoff::value( int i ) const
{
  updateValues();
  return mData->valueAt( i );
}

double ReosRunoff::incrementalValue( int i )
{
  updateValues();
  return mData->valueWithMode( i, ReosTimeSerieConstantInterval::Value );
}

void ReosRunoff::updateValues() const
{
  if ( !isObsolete() )
    return;

  mData->clear();

  if ( !mRainfall.isNull() && mRunoffModelsGroups )
  {
    mData->setReferenceTime( mRainfall->referenceTime() );
    mData->setTimeStep( mRainfall->timeStep() );

    for ( int i = 0; i < mRunoffModelsGroups->runoffModelCount(); ++i )
    {
      ReosRunoffModel *model = mRunoffModelsGroups->runoffModel( i );
      double coefficient = mRunoffModelsGroups->coefficient( i )->value();

      if ( model )
      {
        if ( !model->addRunoffModel( mRainfall, mData, coefficient ) )
        {
          setActualized();
          emit dataChanged();
          return;
        }
      }
    }
    setActualized();
    emit dataChanged();
    return;
  }

  setActualized();
  emit dataChanged();
  return ;
}

ReosTimeSerieConstantInterval *ReosRunoff::data() const
{
  updateValues();
  return mData;
}

ReosRunoffConstantCoefficientModel::ReosRunoffConstantCoefficientModel( const QString &name, QObject *parent ):
  ReosRunoffModel( name, parent ),
  mCoefficient( new ReosParameterDouble( QObject::tr( "Coefficient" ), false, this ) )
{
  mCoefficient->setValue( 0.2 );
  connectParameters();
}


ReosRunoffConstantCoefficientModel::ReosRunoffConstantCoefficientModel( const ReosEncodedElement &element, QObject *parent ):
  ReosRunoffModel( element, parent )
{
  mCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "coefficient" ) ), false, QObject::tr( "Coefficient" ), this );
  connectParameters();
}

QList<ReosParameter *> ReosRunoffConstantCoefficientModel::parameters() const
{
  QList<ReosParameter *> ret;
  ret << name();
  ret << mCoefficient;

  return ret;
}

bool ReosRunoffModel::applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, ReosTimeSerieConstantInterval *runoffResult, double factor )
{
  if ( !rainfall || !runoffResult )
    return false;

  runoffResult->clear();
  runoffResult->insertValues( 0, rainfall->valueCount(), 0.0 );

  return addRunoffModel( rainfall, runoffResult, factor );
}

bool ReosRunoffConstantCoefficientModel::addRunoffModel( ReosTimeSerieConstantInterval *rainfall, ReosTimeSerieConstantInterval *runoffResult, double factor )
{
  if ( !rainfall || !runoffResult )
    return false;
  if ( runoffResult->valueCount() != rainfall->valueCount() )
    return applyRunoffModel( rainfall, runoffResult, factor );

  double coef = mCoefficient->value();

  double *rain = rainfall->data();
  double *runoff = runoffResult->data();

  for ( int i = 0; i < rainfall->valueCount() ; ++i )
    runoff[i] +=  rain[i] * coef * factor;

  return true;
}

ReosParameterDouble *ReosRunoffConstantCoefficientModel::coefficient()
{
  return mCoefficient;
}

ReosEncodedElement ReosRunoffConstantCoefficientModel::encode() const
{
  ReosEncodedElement element( QStringLiteral( "constant-coefficient-runoff-model" ) );
  encodeBase( element );

  element.addEncodedData( QStringLiteral( "coefficient" ), mCoefficient->encode() );

  return element;
}

ReosRunoffConstantCoefficientModel *ReosRunoffConstantCoefficientModel::create( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "constant-coefficient-runoff-model" ) )
    return nullptr;

  return new ReosRunoffConstantCoefficientModel( element, parent );
}

ReosRunoffModelModel::ReosRunoffModelModel( QObject *parent ): QAbstractItemModel( parent )
{}

QModelIndex ReosRunoffModelModel::index( int row, int column, const QModelIndex &parent ) const
{
  if ( parent.isValid() )
  {
    if ( parent.parent().isValid() )
      return QModelIndex();

    QString type = indexToType( parent );
    if ( type.isEmpty() || !mRunoffCollections.contains( type ) )
      return QModelIndex();

    const ReosRunoffModelCollection &runoffColletion = mRunoffCollections[type];
    if ( row >= runoffColletion.runoffModelsCount() )
      return QModelIndex();

    return createIndex( row, column, runoffColletion.runoffModel( row ) );

  }

  return createIndex( row, column, nullptr );
}

QModelIndex ReosRunoffModelModel::parent( const QModelIndex &child ) const
{
  ReosRunoffModel *runoffModel = indexToRunoffModel( child );

  if ( runoffModel )
  {
    QString runoffType = runoffModel->runoffType();
    return typeToIndex( runoffType );
  }

  return QModelIndex();
}

int ReosRunoffModelModel::rowCount( const QModelIndex &parent ) const
{
  if ( !parent.isValid() )
    return mRunoffCollections.count();

  if ( indexToRunoffModel( parent ) )
    return 0;

  QString runoffType = indexToType( parent );

  if ( mRunoffCollections.contains( runoffType ) )
    return mRunoffCollections[runoffType].runoffModelsCount();

  return 0;
}

QVariant ReosRunoffModelModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    if ( !index.parent().isValid() )
      return mRunoffCollections.value( indexToType( index ) ).displayedText();

    ReosRunoffModel *runoffModel = indexToRunoffModel( index );
    if ( runoffModel )
      return runoffModel->name()->value();
  }

  if ( role == Qt::DecorationRole )
  {
    if ( !index.parent().isValid() )
      return mRunoffCollections.value( indexToType( index ) ).icon();
  }

  return QVariant();
}

bool ReosRunoffModelModel::addModel( ReosRunoffModel *runoffModel )
{
  if ( !runoffModel )
    return false;
  const QString &runoffType = runoffModel->runoffType();
  if ( mRunoffCollections.contains( runoffType ) )
  {
    ReosRunoffModelCollection &modelCollection = mRunoffCollections[runoffType];
    beginInsertRows( typeToIndex( runoffType ), modelCollection.runoffModelsCount(), modelCollection.runoffModelsCount() );
    connect( runoffModel, &ReosRunoffModel::dataChanged, this, &ReosRunoffModelModel::modelChanged );
    modelCollection.addModel( runoffModel );
    endInsertRows();
    return true;
  }

  return false;
}

void ReosRunoffModelModel::addCollection( const QString &type, const QString &displayedName, const QIcon &icon )
{
  if ( mRunoffCollections.contains( type ) )
    return;

  mRunoffCollections[type] = ReosRunoffModelCollection( type, displayedName, icon );
}

ReosRunoffModel *ReosRunoffModelModel::runoffModel( const QModelIndex &index ) const
{
  return indexToRunoffModel( index );
}

bool ReosRunoffModelModel::containsRunoffModel( const QString &name ) const
{
  for ( const ReosRunoffModelCollection &rocol : mRunoffCollections )
    if ( rocol.containsModelRunoff( name ) )
      return true;

  return false;
}

void ReosRunoffModelModel::removeRunoffModel( ReosRunoffModel *runoffModel )
{
  if ( !runoffModel )
    return;

  QString type = runoffModel->runoffType();
  QModelIndex parent = typeToIndex( type );

  if ( mRunoffCollections.contains( type ) &&
       mRunoffCollections[type].containsModelRunoff( runoffModel->name()->value() ) )
  {
    int pos = mRunoffCollections[type].index( runoffModel );
    if ( pos < 0 )
      return;
    beginRemoveRows( parent, pos, pos );
    mRunoffCollections[type].removeRunoffModel( pos );
    endRemoveRows();
  }
}

QModelIndex ReosRunoffModelModel::typeToIndex( const QString &runoffType ) const
{
  QList<QString> typeList = mRunoffCollections.keys();

  int position = typeList.indexOf( runoffType );
  if ( position >= 0 )
    return index( position, 0, QModelIndex() );
  else
    return QModelIndex();
}

QString ReosRunoffModelModel::indexToType( const QModelIndex &index ) const
{
  if ( !index.isValid() )
    return QString();

  if ( index.parent().isValid() )
    return indexToType( index.parent() );

  QList<QString> typeList = mRunoffCollections.keys();
  if ( index.row() >= typeList.count() )
    return QString();

  return typeList.at( index.row() );
}

QModelIndex ReosRunoffModelModel::runoffModelToIndex( ReosRunoffModel *runoffModel ) const
{
  if ( !runoffModel )
    return QModelIndex();

  QString type = runoffModel->runoffType();

  QModelIndex parent = typeToIndex( type );

  if ( mRunoffCollections.contains( type ) )
  {
    int pos = mRunoffCollections[type].index( runoffModel );
    if ( pos > 0 )
      return index( pos, 0, parent );
  }

  return QModelIndex();
}

bool ReosRunoffModelModel::hasData() const
{
  for ( const ReosRunoffModelCollection &roCol : mRunoffCollections )
    if ( roCol.runoffModelsCount() != 0 )
      return true;

  return false;
}

void ReosRunoffModelModel::clear()
{
  beginResetModel();
  for ( ReosRunoffModelCollection &roCol : mRunoffCollections )
    roCol.clearCollection();
  endResetModel();
}

QList<ReosEncodedElement> ReosRunoffModelModel::encodeModels() const
{
  QList<ReosEncodedElement> ret;
  for ( const ReosRunoffModelCollection &roCol : mRunoffCollections )
  {
    for ( int i = 0; i < roCol.runoffModelsCount() ; ++i )
      ret.append( roCol.runoffModel( i )->encode() );
  }

  return ret;
}

ReosRunoffModel *ReosRunoffModelModel::runoffModelByUniqueId( const QString &uniqueId ) const
{
  for ( const ReosRunoffModelCollection &roCol : mRunoffCollections )
  {
    ReosRunoffModel *ro = roCol.runoffModelByUniqueId( uniqueId );
    if ( ro )
      return ro;
  }

  return nullptr;
}

QList<ReosRunoffModel *> ReosRunoffModelModel::runofModels( const QString &type )
{
  if ( mRunoffCollections.contains( type ) )
    return mRunoffCollections[type].models();
  else
    return QList<ReosRunoffModel *>();
}

QStringList ReosRunoffModelModel::collectionTypes() const
{
  return mRunoffCollections.keys();
}

ReosRunoffModelCollection ReosRunoffModelModel::runoffModelCollection( const QString &type ) const
{
  return mRunoffCollections.value( type );
}

ReosRunoffModel *ReosRunoffModelModel::indexToRunoffModel( const QModelIndex &index ) const
{
  return static_cast<ReosRunoffModel *>( index.internalPointer() );
}

int ReosRunoffModelModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

ReosRunoffModelRegistery *ReosRunoffModelRegistery::sRegisteryInstance = nullptr;

void ReosRunoffModelRegistery::instantiate( ReosModule *parent )
{
  if ( !sRegisteryInstance )
    sRegisteryInstance = new ReosRunoffModelRegistery( parent );
}

bool ReosRunoffModelRegistery::isInstantiate() {return sRegisteryInstance != nullptr;}

ReosRunoffModelRegistery *ReosRunoffModelRegistery::instance()
{
  return sRegisteryInstance;
}

void ReosRunoffModelRegistery::addModelCollection( const QString &type, const QString displayedText, const QIcon &icon )
{
  mModel->addCollection( type, displayedText, icon );
}

void ReosRunoffModelRegistery::addDescription( const QString &type, const QString description )
{
  mModelDescriptions[type] = description;
}

QString ReosRunoffModelRegistery::modelDescription( const QString &type ) const
{
  return mModelDescriptions.value( type );
}

ReosRunoffModel *ReosRunoffModelRegistery::createModel( const QString &type, const QString &name )
{
  std::unique_ptr<ReosRunoffModel> runoffModel;
  if ( type == QStringLiteral( "constant-coefficient" ) )
    runoffModel.reset( new ReosRunoffConstantCoefficientModel( name, this ) );

  if ( type == QStringLiteral( "green-ampt" ) )
    runoffModel.reset( new ReosRunoffGreenAmptModel( name, this ) );

  if ( type == QStringLiteral( "curve-number" ) )
    runoffModel.reset( new ReosRunoffCurveNumberModel( name, this ) );

  if ( mModel->addModel( runoffModel.get() ) )
    return runoffModel.release();

  return nullptr;
}

ReosRunoffModel *ReosRunoffModelRegistery::createModel( const ReosEncodedElement &element )
{
  std::unique_ptr<ReosRunoffModel> runoffModel;

  if ( element.description() == QStringLiteral( "constant-coefficient-runoff-model" ) )
    runoffModel.reset( ReosRunoffConstantCoefficientModel::create( element, this ) );

  if ( element.description() == QStringLiteral( "green-ampt-runoff-model" ) )
    runoffModel.reset( ReosRunoffGreenAmptModel::create( element, this ) );

  if ( element.description() == QStringLiteral( "curve-number-runoff-model" ) )
    runoffModel.reset( ReosRunoffCurveNumberModel::create( element, this ) );

  if ( runoffModel )
    return runoffModel.release();
  else
    return nullptr;
}

ReosRunoffModelModel *ReosRunoffModelRegistery::model() const {return mModel;}

QString ReosRunoffModelRegistery::createRunoffModelName( const QString &type )
{
  QString name;
  int suffix = 1;
  while ( true )
  {
    if ( type == QStringLiteral( "constant-coefficient" ) )
      name = tr( "Constant coefficient %1" ).arg( suffix );
    else if ( type == QStringLiteral( "green-ampt" ) )
      name = tr( "Green Ampt %1" ).arg( suffix );
    else if ( type == QStringLiteral( "curve-number" ) )
      name = tr( "Curve Number %1" ).arg( suffix );
    else
      name = tr( "Undefined type %1" ).arg( suffix );

    if ( mModel->containsRunoffModel( name ) )
      ++suffix;
    else
      return name;
  }
}

ReosEncodedElement ReosRunoffModelRegistery::encode() const
{
  ReosEncodedElement element( QStringLiteral( "runoff-models-registery" ) );
  element.addListEncodedData( QStringLiteral( "runoff-models" ), mModel->encodeModels() );
  return element;
}

bool ReosRunoffModelRegistery::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "runoff-models-registery" ) )
    return false;

  QList<ReosEncodedElement> runoffModelsEncoded = element.getListEncodedData( QStringLiteral( "runoff-models" ) );

  for ( const ReosEncodedElement &elem : std::as_const( runoffModelsEncoded ) )
    mModel->addModel( createModel( elem ) );

  return true;
}

bool ReosRunoffModelRegistery::saveToFile( const QString &fileName ) const
{
  QFileInfo fileInfo( fileName );

  if ( fileInfo.exists() )
  {
    QFile file( fileName );
    file.copy( fileName + QStringLiteral( ".bck" ) );
  }

  QFile file( fileName );

  QDataStream stream( &file );
  if ( file.open( QIODevice::WriteOnly ) )
  {
    //**** bytes header
    qint32 magicNumber = FILE_MAGIC_NUMBER;
    qint32 serialisationVersion = stream.version();

    QByteArray versionBytes = ReosVersion::currentApplicationVersion().bytesVersion();

    stream << magicNumber;
    stream << serialisationVersion;
    stream << versionBytes;
    //*****

    stream << encode().bytes();
    file.close();
    message( tr( "Runoff models save to file: %1" ).arg( fileName ) );

    return true;
  }

  return false;
}

bool ReosRunoffModelRegistery::loadFromFile( const QString &fileName )
{
  mModel->clear();

  QFileInfo fileInfo( fileName );

  if ( !fileInfo.exists() )
    return false;

  QFile file( fileName );
  QDataStream stream( &file );

  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  //*** read header
  ReosVersion version;
  qint32 magicNumber;
  qint32 serialisationVersion;
  QByteArray bytesVersion;
  stream >> magicNumber;

  if ( magicNumber == FILE_MAGIC_NUMBER )
  {
    // since Lekan 2.2
    stream >> serialisationVersion;
    stream >> bytesVersion;
    QDataStream::Version v = static_cast<QDataStream::Version>( serialisationVersion );
    ReosEncodedElement::setSerialisationVersion( v );
    version = ReosVersion( bytesVersion, v );
  }
  else
  {
    //old version don't have real header but a text header
    ReosEncodedElement::setSerialisationVersion( QDataStream::Qt_5_12 ); /// TODO : check the Qt version of Lekan 2.0 / 2.1
    stream.device()->reset();

    QString readenHeader;
    stream >> readenHeader;
  }

  QByteArray data;
  stream >> data;

  ReosEncodedElement dataElement( data );

  bool result = decode( dataElement );

  if ( result )
    message( tr( "Runoff data loaded from file: %1" ).arg( fileName ) );
  else
    warning( tr( "Unable to load runoff data from file: %1" ).arg( fileName ) );

  return result;
}

ReosRunoffModel *ReosRunoffModelRegistery::runoffModelByUniqueId( const QString &uniqueId ) const
{
  if ( mModel )
    return mModel->runoffModelByUniqueId( uniqueId );
  return nullptr;
}

QStringList ReosRunoffModelRegistery::runoffTypes() const
{
  return mModel->collectionTypes();
}

ReosRunoffModelCollection ReosRunoffModelRegistery::runoffModelCollection( const QString &type ) const
{
  return mModel->runoffModelCollection( type );
}

ReosRunoffModelRegistery::ReosRunoffModelRegistery( QObject *parent ):
  ReosModule( parent )
  , mModel( new ReosRunoffModelModel( this ) )
{
  addModelCollection( QStringLiteral( "constant-coefficient" ),
                      tr( "Constant coefficient" ),
                      QIcon( QStringLiteral( ":/images/runoffConstantCoefficient.svg" ) ) );



  addModelCollection( QStringLiteral( "green-ampt" ),
                      tr( "Green Ampt" ),
                      QIcon( QStringLiteral( ":/images/runoffGreenAmpt.svg" ) ) );

  addModelCollection( QStringLiteral( "curve-number" ),
                      tr( "Curve Number" ),
                      QIcon( QStringLiteral( ":/images/runoffCurveNumber.svg" ) ) );

  addDescription( QStringLiteral( "constant-coefficient" ), tr( "A constant coefficient between 0 and 1\nis applied on the rainfall" ) );
  addDescription( QStringLiteral( "green-ampt" ), tr( "The Green Ampt approach for runoff" ) );
  addDescription( QStringLiteral( "curve-number" ), tr( "Curve Number runoff" ) );
}

ReosRunoffModelCollection::ReosRunoffModelCollection( const QString &type, const QString &displayedText, const QIcon &icon ):
  mType( type )
  , mDisplayedText( displayedText )
  , mIcon( icon )
{}

QString ReosRunoffModelCollection::type() const {return mType;}

QString ReosRunoffModelCollection::displayedText() const
{
  return mDisplayedText;
}

int ReosRunoffModelCollection::runoffModelsCount() const {return mRunoffModels.count();}

void ReosRunoffModelCollection::addModel( ReosRunoffModel *runoffModel )
{
  mRunoffModels.append( runoffModel );
}

void ReosRunoffModelCollection::removeRunoffModel( int i )
{
  mRunoffModels.takeAt( i )->deleteLater();
}

int ReosRunoffModelCollection::index( ReosRunoffModel *runoffModel ) const
{
  return mRunoffModels.indexOf( runoffModel );
}

ReosRunoffModel *ReosRunoffModelCollection::runoffModel( int i ) const
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return nullptr;

  return mRunoffModels.at( i );
}

bool ReosRunoffModelCollection::containsModelRunoff( const QString &name ) const
{
  for ( ReosRunoffModel *ro : mRunoffModels )
    if ( ro && ro->name()->value() == name )
      return true;

  return false;
}

void ReosRunoffModelCollection::clearCollection()
{
  while ( !mRunoffModels.isEmpty() )
    mRunoffModels.takeLast()->deleteLater();
}

ReosRunoffModel *ReosRunoffModelCollection::runoffModelByUniqueId( const QString &uniqueId ) const
{
  for ( ReosRunoffModel *ro : std::as_const( mRunoffModels ) )
    if ( ro->uniqueId() == uniqueId )
      return ro;

  return nullptr;
}

QList<ReosRunoffModel *> ReosRunoffModelCollection::models() const
{
  return mRunoffModels;
}

ReosRunoffModelsGroup::ReosRunoffModelsGroup( QObject *parent ):
  ReosDataObject( parent ) {}

void ReosRunoffModelsGroup::addRunoffModel( ReosRunoffModel *runoffModel )
{
  ReosParameterDouble *paramPortion = new ReosParameterDouble( tr( "Portion" ), false, this );
  paramPortion->setValue( sharePortion() );
  mRunoffModels.append( {QPointer<ReosRunoffModel>( runoffModel ), paramPortion, false} );

  connectModel( mRunoffModels.count() - 1 );

  emit dataChanged();
}

void ReosRunoffModelsGroup::replaceRunnofModel( int i, ReosRunoffModel *runoffModel )
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return;


  std::get<0>( mRunoffModels[i] ) = runoffModel;
  connectModel( i );
  emit dataChanged();
}

int ReosRunoffModelsGroup::runoffModelCount() const {return mRunoffModels.count();}

void ReosRunoffModelsGroup::removeRunoffModel( int i )
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return;

  double coefficientToDispatch = coefficient( i )->value();
  disconnectModel( i );
  std::get<1>( mRunoffModels.at( i ) )->deleteLater();
  mRunoffModels.removeAt( i );
  dispatch( coefficientToDispatch );

  emit dataChanged();
}

void ReosRunoffModelsGroup::lock( int i, bool b )
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return;
  std::get<2>( mRunoffModels[i] ) = b;
}

bool ReosRunoffModelsGroup::isLocked( int i ) const
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return false;
  return std::get<2>( mRunoffModels[i] );
}

ReosRunoffModel *ReosRunoffModelsGroup::runoffModel( int i ) const
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return nullptr;

  if ( std::get<0>( mRunoffModels.at( i ) ).isNull() )
    return nullptr;

  return std::get<0>( mRunoffModels.at( i ) );
}

ReosParameterDouble *ReosRunoffModelsGroup::coefficient( int i ) const
{

  if ( i < 0 || i >= mRunoffModels.count() )
    return nullptr;

  return std::get<1>( mRunoffModels.at( i ) );
}

void ReosRunoffModelsGroup::clear()
{
  blockSignals( true );
  while ( runoffModelCount() > 0 )
  {
    removeRunoffModel( 0 );
  }
  blockSignals( false );

  emit dataChanged();
}

ReosEncodedElement ReosRunoffModelsGroup::encode() const
{
  ReosEncodedElement element( QStringLiteral( "watershed-runoff-models" ) );
  QList<ReosEncodedElement> encodedElements;

  for ( int i = 0; i < runoffModelCount(); ++i )
  {
    ReosEncodedElement elem( QStringLiteral( "watershed-runoff-model" ) );
    ReosRunoffModel *ro = runoffModel( i );
    if ( ro )
      elem.addData( QStringLiteral( "runoff-unique-id" ), ro->uniqueId() );
    else
      elem.addData( QStringLiteral( "runoff-unique-id" ), QStringLiteral( "none" ) );

    elem.addEncodedData( QStringLiteral( "watershed-portion" ), coefficient( i )->encode() );
    elem.addData( QStringLiteral( "locked" ), isLocked( i ) ? 1 : 0 );
    encodedElements.append( elem );
  }

  element.addListEncodedData( QStringLiteral( "models" ), encodedElements );

  return element;
}

void ReosRunoffModelsGroup::decode( const ReosEncodedElement &element )
{
  if ( !ReosRunoffModelRegistery::isInstantiate() )
    return;

  ReosRunoffModelRegistery *registery = ReosRunoffModelRegistery::instance();

  blockSignals( true );
  clear();
  if ( element.description() == QStringLiteral( "watershed-runoff-models" ) )
  {
    QList<ReosEncodedElement> encodedModels = element.getListEncodedData( QStringLiteral( "models" ) );

    for ( const ReosEncodedElement &elem : std::as_const( encodedModels ) )
    {
      if ( elem.description() != QStringLiteral( "watershed-runoff-model" ) )
        continue;

      QString roId;
      elem.getData( QStringLiteral( "runoff-unique-id" ), roId );
      ReosRunoffModel *ro = registery->runoffModelByUniqueId( roId );
      int l = 0;
      elem.getData( QStringLiteral( "locked" ), l );
      ReosParameterDouble *paramPortion = ReosParameterDouble::decode( elem.getEncodedData( "watershed-portion" ), false, tr( "Portion" ), this );
      mRunoffModels.append( {QPointer<ReosRunoffModel>( ro ), paramPortion, l == 1} );
      connectModel( mRunoffModels.count() - 1 );
    }
  }

  blockSignals( false );
  emit dataChanged();
}


double ReosRunoffModelsGroup::sharePortion()
{
  blockSignals( true );

  double sharablePortion = 1;
  int generousOne = 0;

  for ( int i = 0; i < runoffModelCount(); ++i )
  {
    if ( isLocked( i ) )
      sharablePortion -= coefficient( i )->value();
    else
      generousOne++;
  }

  double givenPortion = sharablePortion / ( generousOne + 1 );

  //! adjust the generous
  for ( int i = 0; i < runoffModelCount(); ++i )
  {
    if ( !isLocked( i ) )
    {
      double old = coefficient( i )->value();
      double taken = old / sharablePortion * givenPortion;
      coefficient( i )->setValue( old - taken );
    }
  }

  blockSignals( false );

  return givenPortion;
}

void ReosRunoffModelsGroup::dispatch( double coefToDispatch )
{
  double availableToDispatch = coefToDispatch;
  for ( int i = 0; i < runoffModelCount(); ++i )
  {
    if ( isLocked( i ) )
      continue;
    double oldValue = coefficient( i )->value();
    double toGive = std::max( std::min( availableToDispatch, ( 1 - oldValue ) ), 0.0 );
    coefficient( i )->setValue( oldValue + toGive );
    availableToDispatch -= toGive;
  }
}

void ReosRunoffModelsGroup::connectModel( int i )
{
  connect( runoffModel( i ), &ReosDataObject::dataChanged, this, &ReosDataObject::dataChanged );
  connect( coefficient( i ), &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

void ReosRunoffModelsGroup::disconnectModel( int i )
{
  disconnect( runoffModel( i ), &ReosDataObject::dataChanged, this, &ReosDataObject::dataChanged );
  disconnect( coefficient( i ), &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosRunoffGreenAmptModel::ReosRunoffGreenAmptModel( const QString &name, QObject *parent ):
  ReosRunoffModel( name, parent )
  , mInitialRetentionParameter( new ReosParameterDouble( tr( "Initial retention (mm)" ), false, this ) )
  , mSaturatedPermeabilityParameter( new ReosParameterDouble( tr( "Saturated permeability (mm/h)" ), false, this ) )
  , mSoilPorosityParameter( new ReosParameterDouble( tr( "Soil porosity (vol/vol)" ), false, this ) )
  , mInitialWaterContentParameter( new ReosParameterDouble( tr( "Initial water content (vol/vol)" ), false, this ) )
  , mWettingFrontSuctionParameter( new ReosParameterDouble( tr( "Wetting front suction (mm)" ), false, this ) )
{
  mInitialRetentionParameter->setValue( 0.0 );
  mSaturatedPermeabilityParameter->setValue( 15 );
  mSoilPorosityParameter->setValue( 0.35 );
  mInitialWaterContentParameter->setValue( 0.25 );
  mWettingFrontSuctionParameter->setValue( 600 );

  connectParameters();
}

QList<ReosParameter *> ReosRunoffGreenAmptModel::parameters() const
{
  QList<ReosParameter *> ret;
  ret << name();
  ret << mInitialRetentionParameter;
  ret << mSaturatedPermeabilityParameter;
  ret << mSoilPorosityParameter;
  ret << mInitialWaterContentParameter;
  ret << mWettingFrontSuctionParameter;

  return ret;
}

double static resolveFpEquation( double T, double SM, double K )
{
  double X1;
  double X2 = 1;
  int it = 0;
  do
  {
    X1 = X2;
    X2 = X1 - ( X1 - log( 1 + X1 ) - K / SM * T ) / ( 1 - 1 / ( 1 + X1 ) );
    it++;
  }
  while ( fabs( ( X2 - X1 ) / X2 ) > 0.001 && it < 100 );

  return X2 * SM;
}

bool ReosRunoffGreenAmptModel::addRunoffModel( ReosTimeSerieConstantInterval *rainfall, ReosTimeSerieConstantInterval *runoffResult, double factor )
{
  if ( !rainfall || !runoffResult )
    return false;
  if ( runoffResult->valueCount() != rainfall->valueCount() )
    return applyRunoffModel( rainfall, runoffResult, factor );

  double *rain = rainfall->data();
  double *runoff = runoffResult->data();

  int n = rainfall->valueCount();
  double Ia = mInitialRetentionParameter->value();
  double K = mSaturatedPermeabilityParameter->value();
  double P = 0;
  double Rprev = 0;
  double R = 0;
//  double rm = 0;
  double F = 0;
  double Fp = 0;
  double I = 0;
  double KSMIK = std::numeric_limits<double>::max();
  double SM = ( mSoilPorosityParameter->value() - mInitialWaterContentParameter->value() ) * mWettingFrontSuctionParameter->value();
  double cu = -10;
  double cp = -10;
  double tp = 0;
  double Pp;
  double ts = 0;
  bool pondingInitial = false;
  bool pondingTerminal = false;
  ReosDuration timeStep = rainfall->timeStepParameter()->value();
  double timeStepHour = timeStep.valueHour();

  double K_h = K * timeStepHour;

  if ( SM < 0 )
  {
    for ( int i = 0; i < n; ++i )
      if ( rain[i] > K_h )
        runoff[i] += ( rain[i] - K_h ) * factor;
    return true;
  }


  for ( int i = 0; i < rainfall->valueCount() ; ++i )
  {
    double dP = rain[i];
    Rprev = R;
    P += dP;
    I = dP / timeStepHour;
    pondingInitial = pondingTerminal;

    if ( I > K )
    {
      //rm = 0;
      KSMIK = K * SM / ( I - K );
      cu = P - R - KSMIK;


      if ( !pondingInitial )
      {
        pondingTerminal = cu > 0;
      }
      else
      {
        pondingTerminal = true;
      }

      if ( ( !pondingInitial ) && ( pondingTerminal ) )
      {
        double dt = ( KSMIK - P + dP + R ) / I;;
        if ( dt < 0 )
          dt = 0;

        tp = i * timeStepHour + dt;

        Pp = P - dP + I * dt;
        ts = ( ( Pp - R ) / SM - log( 1 + ( Pp - R ) / SM ) ) * SM / K;
      }

      if ( pondingTerminal )
      {
        Fp = resolveFpEquation( ( i + 1 ) * timeStepHour - tp + ts, SM, K );
        cp = P - Fp - R;

        pondingTerminal = cp > 0;

        if ( pondingTerminal )
        {
          F = Fp;
          if ( P - Fp - Ia > R )
          {
            R = P - Fp - Ia;
          }
        }
//        else
//        {
//          F = P - R;
//        }
      }
//      if ( !pondingInitial && !pondingTerminal )
//      {
//        F = P - R;
//      }
    }
    else
    {
//      F = P - R;
      pondingTerminal = false;
    }

    runoff[i] += ( R - Rprev ) * factor;
  }


  return true;
}

ReosEncodedElement ReosRunoffGreenAmptModel::encode() const
{
  ReosEncodedElement element( QStringLiteral( "green-ampt-runoff-model" ) );
  encodeBase( element );

  element.addEncodedData( QStringLiteral( "initial-retention" ), mInitialRetentionParameter->encode() );
  element.addEncodedData( QStringLiteral( "saturated-permeability" ), mSaturatedPermeabilityParameter->encode() );
  element.addEncodedData( QStringLiteral( "soil-porosity" ), mSoilPorosityParameter->encode() );
  element.addEncodedData( QStringLiteral( "initial-water-content" ), mInitialWaterContentParameter->encode() );
  element.addEncodedData( QStringLiteral( "wetting-front-succion" ), mWettingFrontSuctionParameter->encode() );

  return element;
}

ReosRunoffGreenAmptModel *ReosRunoffGreenAmptModel::create( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "green-ampt-runoff-model" ) )
    return nullptr;

  return new ReosRunoffGreenAmptModel( element, parent );
}

ReosRunoffGreenAmptModel::ReosRunoffGreenAmptModel( const ReosEncodedElement &element, QObject *parent ):
  ReosRunoffModel( element, parent )
{
  mInitialRetentionParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "initial-retention" ) ), false, tr( "Initial retention (mm)" ), this );
  mSaturatedPermeabilityParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "saturated-permeability" ) ), false, tr( "Saturated permeability (mm/h)" ), this );
  mSoilPorosityParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "soil-porosity" ) ), false, tr( "Soil porosity (vol/vol)" ), this );
  mInitialWaterContentParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "initial-water-content" ) ), false, tr( "Initial water content (vol/vol)" ), this );
  mWettingFrontSuctionParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "wetting-front-succion" ) ), false, tr( "Wetting front suction (mm)" ), this );
  connectParameters();
}

ReosRunoffCurveNumberModel::ReosRunoffCurveNumberModel( const QString &name, QObject *parent ):
  ReosRunoffModel( name, parent )
  , mCurveNumberParameter( new ReosParameterDouble( tr( "Curve number" ), false, this ) )
  , mInitialRetentionParameter( new ReosParameterDouble( tr( "Initial retention (mm)" ), false, this ) )
  , mInitialRetentionFromS( new ReosParameterBoolean( tr( "Initial retention : 0.2 x S" ) ) )
{
  mCurveNumberParameter->setValue( 80.0 );
  mInitialRetentionParameter->setValue( 0.0 );
  mInitialRetentionFromS->setValue( true );

  connectParameters();
}

QList<ReosParameter *> ReosRunoffCurveNumberModel::parameters() const
{
  QList<ReosParameter *> ret;
  ret << name();
  ret << mCurveNumberParameter;
  ret << mInitialRetentionFromS;
  ret << mInitialRetentionParameter;

  return ret;
}

bool ReosRunoffCurveNumberModel::addRunoffModel( ReosTimeSerieConstantInterval *rainfall, ReosTimeSerieConstantInterval *runoffResult, double factor )
{
  if ( !rainfall || !runoffResult )
    return false;
  if ( runoffResult->valueCount() != rainfall->valueCount() )
    return applyRunoffModel( rainfall, runoffResult, factor );

  if ( mCurveNumberParameter->value() <= 0 )
    return true;

  int n = rainfall->valueCount();

  double P = 0;
  double R = 0;
  double S = 25400 / mCurveNumberParameter->value() - 254;
  double Ia;
  if ( mInitialRetentionFromS )
    Ia = 0.2 * S;
  else
    Ia = mInitialRetentionParameter->value();

  double *rain = rainfall->data();
  double *runoff = runoffResult->data();

  for ( int i = 0; i < n; ++i )
  {
    double dP = rain[i];
    double Rprev = R;
    P += dP;
    if ( P < Ia )
      R = 0;
    else
    {
      R = pow( P - Ia, 2 ) / ( P - Ia + S );
    }
    runoff[i] += ( R - Rprev ) * factor;
  }

  return true;
}

ReosEncodedElement ReosRunoffCurveNumberModel::encode() const
{
  ReosEncodedElement element( QStringLiteral( "curve-number-runoff-model" ) );
  encodeBase( element );

  element.addEncodedData( QStringLiteral( "curve-number" ), mCurveNumberParameter->encode() );
  element.addEncodedData( QStringLiteral( "initial-retention" ), mInitialRetentionParameter->encode() );
  element.addEncodedData( QStringLiteral( "calculate-initial-retention" ), mInitialRetentionFromS->encode() );

  return element;
}

ReosRunoffCurveNumberModel *ReosRunoffCurveNumberModel::create( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "curve-number-runoff-model" ) )
    return nullptr;

  return new ReosRunoffCurveNumberModel( element, parent );
}

ReosParameterDouble *ReosRunoffCurveNumberModel::curveNumber() const
{
  return mCurveNumberParameter;
}

ReosParameterBoolean *ReosRunoffCurveNumberModel::initialRetentionFromS() const
{
  return mInitialRetentionFromS;
}

ReosParameterDouble *ReosRunoffCurveNumberModel::initialRetention() const
{
  return mInitialRetentionParameter;
}

ReosRunoffCurveNumberModel::ReosRunoffCurveNumberModel( const ReosEncodedElement &element, QObject *parent ):
  ReosRunoffModel( element, parent )
{
  mCurveNumberParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "curve-number" ) ), false, tr( "Curve number" ), this );
  mInitialRetentionParameter = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "initial-retention" ) ), false, tr( "Initial retention (mm)" ), this );
  mInitialRetentionFromS = ReosParameterBoolean::decode( element.getEncodedData( QStringLiteral( "calculate-initial-retention" ) ), false, tr( "Initial retention : 0.2 x S" ), this );
  connectParameters();
}
