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

ReosRunoff::ReosRunoff( ReosRunoffModel *runoffModel, ReosTimeSerieConstantInterval *rainfall, QObject *parent ):
  ReosDataObject( parent )
  , mRainfall( rainfall )
  , mRunoffModel( runoffModel )
{
  connect( mRainfall, &ReosDataObject::dataChanged, this, &ReosRunoff::updateValues );
  connect( runoffModel, &ReosRunoffModel::modelChanged, this, &ReosRunoff::updateValues );
}

int ReosRunoff::valueCount() const
{
  return mData.count();
}

ReosDuration ReosRunoff::timeStep() const
{
  return mRainfall->timeStep()->value();
}

double ReosRunoff::value( int i ) const
{
  return mData.at( i );
}

bool ReosRunoff::updateValues()
{
  if ( !mRainfall.isNull() )
    return mRunoffModel->applyRunoffModel( mRainfall, mData );

  return false;
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
}

QList<ReosParameter *> ReosRunoffConstantCoefficientModel::parameters() const
{
  QList<ReosParameter *> ret;
  ret << name();
  ret << mCoefficient;

  return ret;
}

bool ReosRunoffConstantCoefficientModel::applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, QVector<double> &runoffResult )
{
  if ( !rainfall )
    return false;

  runoffResult.clear();
  runoffResult.resize( rainfall->valueCount() );

  double coef = mCoefficient->value();

  for ( int i = 0; i < rainfall->valueCount() ; ++i )
    runoffResult[i] = rainfall->valueAt( i ) * coef;

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
