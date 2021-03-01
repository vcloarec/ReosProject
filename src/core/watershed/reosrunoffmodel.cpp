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

ReosRunoffModel::ReosRunoffModel( const QString &name, QObject *parent ):
  QObject( parent )
  , mName( new ReosParameterString( QObject::tr( "Name" ), false, this ) )
{
  mName->setValue( name );
  mUniqueId = QUuid::createUuid().toString();
}

ReosRunoffModel::ReosRunoffModel( const ReosEncodedElement &element, QObject *parent ):
  QObject( parent )
{
  mName = ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, this );
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
    connect( params.at( i ), &ReosParameter::valueChanged, this, &ReosRunoffModel::modelChanged );
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
  mData->copyAttribute( rainfall );
  mData->syncWith( rainfall );

  connect( runoffModels, &ReosDataObject::dataChanged, this, &ReosRunoff::updateValues );
  connect( rainfall, &ReosDataObject::dataChanged, this, &ReosRunoff::updateValues );
}

int ReosRunoff::valueCount() const
{
  return mData->valueCount();
}

ReosDuration ReosRunoff::timeStep() const
{
  return mRainfall->timeStep()->value();
}

double ReosRunoff::value( int i ) const
{
  return mData->valueAt( i );
}

double ReosRunoff::incrementalValue( int i )
{
  return mData->valueWithMode( i, ReosTimeSerieConstantInterval::Value );
}

bool ReosRunoff::updateValues()
{
  if ( !mRainfall.isNull() && mRunoffModelsGroups )
  {
    mData->clear();
    for ( int i = 0; i < mRunoffModelsGroups->runoffModelCount(); ++i )
    {
      ReosRunoffModel *model = mRunoffModelsGroups->runoffModel( i );
      double coefficient = mRunoffModelsGroups->coefficient( i )->value();

      if ( model )
      {
        if ( !model->addRunoffModel( mRainfall, mData, coefficient ) )
        {
          emit dataChanged();
          return false;
        }
      }
    }
    emit dataChanged();
    return true;
  }

  emit dataChanged();
  return false;
}

ReosTimeSerieConstantInterval *ReosRunoff::data() const
{
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
  mCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "coefficient" ) ), false, this );
  connectParameters();
}

QList<ReosParameter *> ReosRunoffConstantCoefficientModel::parameters() const
{
  QList<ReosParameter *> ret;
  ret << name();
  ret << mCoefficient;

  return ret;
}

bool ReosRunoffConstantCoefficientModel::applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, ReosTimeSerieConstantInterval *runoffResult, double factor )
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
    modelCollection.addModel( runoffModel );
    endInsertRows();
    return true;
  }

  return false;
}

void ReosRunoffModelModel::addCollection( const QString &type, const QString &displayedName, const QPixmap &icon )
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

void ReosRunoffModelRegistery::addModelCollection( const QString &type, const QString displayedText, const QPixmap &icon )
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

  if ( mModel->addModel( runoffModel.get() ) )
    return runoffModel.release();

  return nullptr;
}

ReosRunoffModel *ReosRunoffModelRegistery::createModel( const ReosEncodedElement &element )
{
  std::unique_ptr<ReosRunoffModel> runoffModel;

  if ( element.description() == QStringLiteral( "constant-coefficient-runoff-model" ) )
  {
    runoffModel.reset( ReosRunoffConstantCoefficientModel::create( element, this ) );
  }

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

  for ( const ReosEncodedElement &elem : qAsConst( runoffModelsEncoded ) )
    mModel->addModel( createModel( elem ) );

  return true;
}

bool ReosRunoffModelRegistery::saveToFile( const QString &fileName, const QString &header ) const
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
    stream << header;
    stream << encode().bytes();
    file.close();
    message( tr( "Runoff models save to file: %1" ).arg( fileName ) );

    return true;
  }

  return false;
}

bool ReosRunoffModelRegistery::loadFromFile( const QString &fileName, const QString &header )
{
  Q_UNUSED( header );

  mModel->clear();

  QFileInfo fileInfo( fileName );

  if ( !fileInfo.exists() )
    return false;

  QFile file( fileName );
  QDataStream stream( &file );

  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QString readenHeader;
  stream >> readenHeader;

  QByteArray data;
  stream >> data;

  ReosEncodedElement dataElement( data );

  return decode( data );
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
                      QPixmap( QStringLiteral( ":/images/runoffConstantCoefficient.svg" ) ) );

  addDescription( QStringLiteral( "constant-coefficient" ), tr( "A constant coefficient between 0 and 1\nis applied on the rainfall" ) );
}

ReosRunoffModelCollection::ReosRunoffModelCollection( const QString &type, const QString &displayedText, const QPixmap &icon ):
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
  for ( ReosRunoffModel *ro : qAsConst( mRunoffModels ) )
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

  connect( runoffModel, &ReosRunoffModel::modelChanged, this, &ReosDataObject::dataChanged );
  connect( paramPortion, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );

  emit dataChanged();
}

void ReosRunoffModelsGroup::replaceRunnofModel( int i, ReosRunoffModel *runoffModel )
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return;

  std::get<0>( mRunoffModels[i] ) = runoffModel;

  emit dataChanged();
}

int ReosRunoffModelsGroup::runoffModelCount() const {return mRunoffModels.count();}

void ReosRunoffModelsGroup::removeRunoffModel( int i )
{
  if ( i < 0 || i >= mRunoffModels.count() )
    return;

  double coefficientToDispatch = coefficient( i )->value();
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
    removeRunoffModel( 0 );
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

    for ( const ReosEncodedElement &elem : qAsConst( encodedModels ) )
    {
      if ( elem.description() != QStringLiteral( "watershed-runoff-model" ) )
        continue;

      QString roId;
      elem.getData( QStringLiteral( "runoff-unique-id" ), roId );
      ReosRunoffModel *ro = registery->runoffModelByUniqueId( roId );
      int l = 0;
      elem.getData( QStringLiteral( "locked" ), l );
      ReosParameterDouble *paramPortion = ReosParameterDouble::decode( elem.getEncodedData( "watershed-portion" ), false, this );
      mRunoffModels.append( {QPointer<ReosRunoffModel>( ro ), paramPortion, l == 1} );
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
