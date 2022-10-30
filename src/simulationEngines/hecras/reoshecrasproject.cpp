#include "reoshecrasproject.h"

#include <QTextStream>
#include <QFile>
#include <QFileInfo>
#include <QDir>

ReosHecRasProject::ReosHecRasProject( const QString &fileName ):
  mFileName( fileName )
{
  parseProjectFile();
}

QString ReosHecRasProject::currentPlanId() const
{
  return mCurrentPlan;
}

QStringList ReosHecRasProject::planIds() const
{
  return mPlans.keys();
}

QString ReosHecRasProject::planTitle( const QString &id ) const
{
  return mPlans.value( id ).title();
}

ReosHecRasPlan ReosHecRasProject::plan( const QString &planId ) const
{
  return mPlans.value( planId );
}

int ReosHecRasProject::GeometriesCount() const
{
  return mGeometries.count();
}

QStringList ReosHecRasProject::geometryIds() const
{
  return mGeometries.keys();
}

ReosHecRasGeometry ReosHecRasProject::geometry( const QString &id ) const
{
  return mGeometries.value( id );
}

ReosHecRasGeometry ReosHecRasProject::currentGeometry() const
{
  const ReosHecRasPlan &currentPlan = mPlans.value( mCurrentPlan );
  return mGeometries.value( currentPlan.geometryFile() );
}

void ReosHecRasProject::parseProjectFile()
{
  QFile file( mFileName );
  if ( !file.open( QIODevice::ReadOnly ) )
    return;

  QFileInfo fileInfo( mFileName );
  QString projectName = fileInfo.baseName();
  QDir projectDir = fileInfo.dir();

  QTextStream stream( &file );

  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine();
    if ( line.startsWith( QStringLiteral( "Geom File=" ) ) )
    {
      QString geomFile = line;
      geomFile.remove( QStringLiteral( "Geom File=" ) );
      mGeometries.insert( geomFile, ReosHecRasGeometry( projectDir.filePath( projectName + '.' + geomFile ) ) );
    }

    if ( line.startsWith( QStringLiteral( "Plan File=" ) ) )
    {
      QString planFile = line;
      planFile.remove( QStringLiteral( "Plan File=" ) );
      mPlans.insert( planFile, ReosHecRasPlan( projectDir.filePath( projectName + '.' + planFile ) ) );
    }

    if ( line.startsWith( QStringLiteral( "Current Plan=" ) ) )
    {
      QString plan = line;
      plan.remove( QStringLiteral( "Current Plan=" ) );
      mCurrentPlan = plan.trimmed();
    }
  }
}

ReosHecRasGeometry::ReosHecRasGeometry( const QString &fileName )
  : mFileName( fileName )
{
  parseGeometryFile();
}

int ReosHecRasGeometry::area2dCount() const
{
  return mAreas2D.count();
}

QString ReosHecRasGeometry::area2dName( int i ) const
{
  return mAreas2D.at( i ).name;
}

ReosHecRasGeometry::FlowArea2D ReosHecRasGeometry::area2d( int i ) const
{
  return mAreas2D.at( i );
}

QList<ReosHecRasGeometry::BoundaryCondition> ReosHecRasGeometry::boundariesConditions( const QString &area2dName ) const
{
  return mBoundariesConditions.value( area2dName );
}

QList<ReosHecRasGeometry::BoundaryCondition> ReosHecRasGeometry::allBoundariesConditions() const
{
  QList<ReosHecRasGeometry::BoundaryCondition> ret;
  for ( const FlowArea2D &area : mAreas2D )
    ret.append( boundariesConditions( area.name ) );

  return ret;
}

void ReosHecRasGeometry::parseGeometryFile()
{
  QFile file( mFileName );
  if ( !file.open( QIODevice::ReadOnly ) )
    return;

  QTextStream stream( &file );

  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine();
    if ( line.startsWith( QStringLiteral( "Geom Title=" ) ) )
    {
      mTitle = line;
      mTitle.remove( QStringLiteral( "Geom Title=" ) );
    }

    if ( line.startsWith( QStringLiteral( "Storage Area=" ) ) )
    {
      QString name = line;
      name.remove( QStringLiteral( "Storage Area=" ) );
      name = name.split( ',' ).at( 0 );
      parseStorageArea( stream, name.trimmed() );
    }
    if ( line.startsWith( QStringLiteral( "BC Line Name=" ) ) )
    {
      QString name = line;
      name.remove( QStringLiteral( "BC Line Name=" ) );
      parseBoundaryCondition( stream, name.trimmed() );
    }
  }
}

void ReosHecRasGeometry::parseStorageArea( QTextStream &stream, const QString storageName )
{
  QPolygonF surface;
  bool is2D = false;
  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine();
    if ( line.startsWith( QStringLiteral( "Storage Area Surface Line=" ) ) )
    {
      QString countString = line;
      countString.remove( QStringLiteral( "Storage Area Surface Line=" ) );
      countString.remove( ' ' );
      int pointCount = countString.toInt() - 1;
      surface.resize( pointCount );
      for ( int i = 0; i < pointCount; ++i )
      {
        QString pointLine = stream.readLine();
        QString partx = pointLine;
        partx.truncate( 16 );
        partx.remove( ' ' );
        QString party = pointLine.mid( 16 );
        party.remove( ' ' );
        surface[i] = QPointF( partx.toDouble(), party.toDouble() );
      }
    }

    if ( line.startsWith( QStringLiteral( "Storage Area Is2D=" ) ) )
    {
      QString is2DString = line;
      is2DString.remove( QStringLiteral( "Storage Area Is2D=" ) );
      is2D = is2DString.toInt() != 0;
    }

    if ( line.isEmpty() )
      break;

  }

  if ( is2D )
  {
    mAreas2D.append( {storageName, surface} );
  }
}

void ReosHecRasGeometry::parseBoundaryCondition( QTextStream &stream, const QString &bcName )
{
  BoundaryCondition bc;
  bc.name = bcName;
  QString storageArea;
  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine();

    if ( line.startsWith( QStringLiteral( "BC Line Storage Area=" ) ) )
    {
      storageArea = line;
      storageArea.remove( "BC Line Storage Area=" );
      storageArea = storageArea.trimmed();
    }

    if ( line.startsWith( QStringLiteral( "BC Line Middle Position=" ) ) )
    {
      QString midPositionString = line;
      midPositionString.remove( QStringLiteral( "BC Line Middle Position=" ) );
      midPositionString.remove( ' ' );
      QStringList coordStr = midPositionString.split( ',' );
      if ( coordStr.count() != 2 )
        return;
      bc.middlePosition = QPointF( coordStr.at( 0 ).toDouble(), coordStr.at( 1 ).toDouble() );
    }

    if ( line.startsWith( QStringLiteral( "BC Line Text Position=" ) ) )
      break;
  }

  if ( mBoundariesConditions.contains( storageArea ) )
    mBoundariesConditions[storageArea].append( bc );
  else
  {
    QList<BoundaryCondition> bcs;
    bcs << bc;
    mBoundariesConditions.insert( storageArea, bcs );
  }

}

ReosHecRasPlan::ReosHecRasPlan( const QString &fileName )
  : mFileName( fileName )
{
  parsePlanFile();
}

QString ReosHecRasPlan::geometryFile() const
{
  return mGeometryFile;
}

const QString &ReosHecRasPlan::title() const
{
    return mTitle;
}

const QDateTime &ReosHecRasPlan::startTime() const
{
    return mStartTime;
}

const QDateTime &ReosHecRasPlan::endTime() const
{
    return mEndTime;
}

void ReosHecRasPlan::parsePlanFile()
{
  QFile file( mFileName );
  if ( !file.open( QIODevice::ReadOnly ) )
    return;

  QTextStream stream( &file );

  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine();
    if ( line.startsWith( QStringLiteral( "Plan Title=" ) ) )
    {
      mTitle = line;
      mTitle.remove( QStringLiteral( "Plan Title=" ) );
      mTitle = mTitle.trimmed();
    }

    if ( line.startsWith( QStringLiteral( "Geom File=" ) ) )
    {
      mGeometryFile = line;
      mGeometryFile.remove( QStringLiteral( "Geom File=" ) );
      mGeometryFile = mGeometryFile.trimmed();
    }

    if ( line.startsWith( QStringLiteral( "Flow File=" ) ) )
    {
      mFlowFile = line;
      mFlowFile.remove( QStringLiteral( "Flow File=" ) );
      mFlowFile = mFlowFile.trimmed();
    }

    if ( line.startsWith( QStringLiteral( "Simulation Date=" ) ) )
    {
      QString dateLine = line;
      dateLine.remove( QStringLiteral( "Simulation Date=" ) );
      QStringList splitDates = dateLine.trimmed().split( ',' );
      if ( splitDates.count() == 4 )
      {
        bool ok = true;
        QDate startDate = ReosHecRasProject::hecRasDateToDate( splitDates.at( 0 ) );
        ok &= startDate.isValid();
        QTime startTime = QTime::fromString( splitDates.at( 1 ) );
        ok &= startTime.isValid();
        QDate endDate = ReosHecRasProject::hecRasDateToDate( splitDates.at( 2 ) );
        ok &= endDate.isValid();
        QTime endTime = QTime::fromString( splitDates.at( 3 ) );
        ok &= endTime.isValid();

        if ( ok )
        {
          mStartTime = QDateTime( startDate, startTime, Qt::UTC );
          mEndTime = QDateTime( endDate, endTime, Qt::UTC );
        }

      }
    }
  }
}

QDate ReosHecRasProject::hecRasDateToDate( const QString &hecrasDate )
{
  if ( hecrasDate.size() != 9 )
    return QDate();

  QString dayStr = hecrasDate;
  dayStr.resize( 2 );
  bool ok = false;
  int d = dayStr.toInt( &ok );
  if ( !ok )
    return QDate();

  QString monthStr = hecrasDate.mid( 2, 3 ).toLower();
  int m = -1;
  if ( monthStr == QStringLiteral( "jan" ) )
    m = 1;
  if ( monthStr == QStringLiteral( "feb" ) )
    m = 2;
  if ( monthStr == QStringLiteral( "mar" ) )
    m = 3;
  if ( monthStr == QStringLiteral( "apr" ) )
    m = 4;
  if ( monthStr == QStringLiteral( "may" ) )
    m = 5;
  if ( monthStr == QStringLiteral( "jun" ) )
    m = 6;
  if ( monthStr == QStringLiteral( "jul" ) )
    m = 7;
  if ( monthStr == QStringLiteral( "aug" ) )
    m = 8;
  if ( monthStr == QStringLiteral( "sep" ) )
    m = 9;
  if ( monthStr == QStringLiteral( "oct" ) )
    m = 10;
  if ( monthStr == QStringLiteral( "nov" ) )
    m = 11;
  if ( monthStr == QStringLiteral( "dec" ) )
    m = 12;

  if ( m == -1 )
    return QDate();

  QString yearStr = hecrasDate.mid( 5 );
  int y = yearStr.toInt( &ok );
  if ( !ok )
    return QDate();

  return QDate( y, m, d );

}
