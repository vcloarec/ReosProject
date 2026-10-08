/***************************************************************************
  reostelemacboundaries.h - ReosTelemacBoundaries

 ---------------------
 begin                : 4.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reostelemacboundaries.h"

#include <QFile>
#include <QTextStream>
#include <QRegularExpression>

#include "reosmesh.h"

ReosTelemacBoundaries::ReosTelemacBoundaries( ReosMesh *mesh, const QString &boudaryFilePath )
  : mMesh( mesh )
  , mBoundaryFilePath( boudaryFilePath )
{
  if ( !mBoundaryFilePath.isEmpty() )
  {
    populateTelemacBoundaryVertexFromFile();
    populateEnvelopFromTelemacBoundaryVertex();
  }
}

int ReosTelemacBoundaries::boundaryVertexCount() const
{
  return mTelemacBoundaryVertex.count();
}

QList<int> ReosTelemacBoundaries::boundaryVertexIndexes() const
{
  QList<int> ret;

  for ( const TelemacBoundaryLine &bound : mTelemacBoundaryVertex )
    ret.append( bound.vertIndex );

  return ret;
}

QPolygonF ReosTelemacBoundaries::envelop() const
{
  return mEnvelop;
}

QList<ReosHydraulicStructureBoundaryCondition::Type> ReosTelemacBoundaries::boundaryConditionTypes() const
{
  return mBoundaryConditionTypes;
}

QList<QSet<int> > ReosTelemacBoundaries::liquidDomainSegmentIndex() const
{
  return mLiquidDomainSegmentIndex;
}
void ReosTelemacBoundaries::populateTelemacBoundaryVertexFromFile()
{
  QFile file( mBoundaryFilePath );
  if ( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
    return;

  mTelemacBoundaryVertex.clear();
  QTextStream stream( &file );

  //        LIHBOR LIUBOR LIVBOR X.XXX X.XXX X.XXX X.XXX bound.LITBOR X.XXX X.XXX X.XXX vertIndex position in file;
  // example line : 2 2 2  0.000 0.000 0.000 0.000  2  0.000 0.000 0.000          32           2
  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine().trimmed();
    QStringList parts = line.split( QRegularExpression( "\\s+" ), Qt::SkipEmptyParts );
    int a = 1;
    TelemacBoundaryLine boundary;
    boundary.LIHBOR = parts.at( 0 ).toInt();
    boundary.LIUBOR = parts.at( 1 ).toInt();
    boundary.LIVBOR = parts.at( 2 ).toInt();
    boundary.LITBOR = parts.at( 7 ).toInt();
    boundary.vertIndex = parts.at( 11 ).toInt();

    mTelemacBoundaryVertex.append( boundary );
  }
}

void ReosTelemacBoundaries::populateEnvelopFromTelemacBoundaryVertex()
{
  if ( mTelemacBoundaryVertex.isEmpty() || !mMesh )
    return;

  constexpr double epsilon = 1e-10;
  QVector<QPointF> simplified;
  int boundaryVertexCount = mTelemacBoundaryVertex.count();
  int liquidRank = 0;
  int onLiquidBound = false;

  for ( int i = 0; i < boundaryVertexCount; ++i )
  {
    const TelemacBoundaryLine &bound1 = mTelemacBoundaryVertex.at( i );
    const QPointF &p1 = mMesh->vertexPosition( bound1.vertIndex - 1 );
    const TelemacBoundaryLine &bound2 = mTelemacBoundaryVertex.at( ( i + 1 ) % boundaryVertexCount );
    const QPointF &p2 = mMesh->vertexPosition( bound2.vertIndex - 1 );
    const TelemacBoundaryLine &bound3 = mTelemacBoundaryVertex.at( ( i + 2 ) % boundaryVertexCount );
    const QPointF &p3 = mMesh->vertexPosition( bound2.vertIndex - 1 );

    const double v1x = p2.x() - p1.x();
    const double v1y = p2.y() - p1.y();
    const double v2x = p3.x() - p2.x();
    const double v2y = p3.y() - p2.y();
    const double len1Squared = v1x * v1x + v1y * v1y;
    const double len2Squared = v2x * v2x + v2y * v2y;
    bool collinear = false;
    // Handle duplicated consecutive points.
    if ( qFuzzyIsNull( len1Squared ) || qFuzzyIsNull( len2Squared ) )
    {
      collinear = true;
    }
    else
    {
      const double cross = v1x * v2y - v1y * v2x;
      const double crossSquared = cross * cross;
      const double toleranceSquared = epsilon * epsilon * len1Squared * len2Squared;
      collinear = crossSquared <= toleranceSquared;
    }

    bool startLiquid = !( bound2.isSolidBoundary() || bound2.isSameBoundary( bound1 ) );
    bool endLiquid = !( bound2.isSolidBoundary() || bound2.isSameBoundary( bound3 ) );


    if ( startLiquid )
    {
      onLiquidBound = true;
      mBoundaryConditionTypes.append( bound2.boundaryConditionType() );
      mLiquidDomainSegmentIndex.append( QSet<int>() );
    }

    if ( !collinear || startLiquid || endLiquid )
    {
      // p2 is not collinear, therefore keep it.
      simplified.append( p2 );
      if ( onLiquidBound && !startLiquid )
        mLiquidDomainSegmentIndex[mBoundaryConditionTypes.count() - 1].insert( simplified.count() - 2 );
    }

    if ( endLiquid )
      onLiquidBound = false;
  }
  mEnvelop = simplified;

  Q_ASSERT( mLiquidDomainSegmentIndex.count() == mBoundaryConditionTypes.count() );
}

ReosTelemacLiquidBoundaries::ReosTelemacLiquidBoundaries( const QString &liquidBoudaryFilePath )
  : mLiquidBoundaryFilePath( liquidBoudaryFilePath )
{}

void ReosTelemacLiquidBoundaries::parse() const
{
  QFile file( mLiquidBoundaryFilePath );
  if ( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
    return;

  QTextStream stream( &file );

  while ( !stream.atEnd() )
  {
    const QString line = stream.readLine().trimmed();
    if ( line.startsWith( "#" ) || line.isEmpty() )
      continue;

    QStringList parts = line.split( QRegularExpression( "\\s+" ), Qt::SkipEmptyParts );
    if ( parts.count() < 2 )
      continue;

    if ( parts.at( 0 ) == 'T' )
    {
      QStringList seriesHeader = parts.mid( 1 );
      const QString unitline = stream.readLine();
      QStringList units = unitline.split( QRegularExpression( "\\s+" ), Qt::SkipEmptyParts );

      for ( int i = 0; i < seriesHeader.count(); ++i )
      {
        TelemacLiquidBoundary bc;
        bc.header = seriesHeader.at( i );
        bc.unit = units.value( i + 1, QString() );
        bc.rank = bc.header.mid( bc.header.indexOf( '(' ) + 1, bc.header.indexOf( ')' ) - bc.header.indexOf( '(' ) - 1 ).toInt();
        if ( bc.header.startsWith( "Q(" ) )
          bc.type = ReosHydraulicStructureBoundaryCondition::Type::InputFlow;
        else if ( bc.header.startsWith( "SL(" ) )
          bc.type = ReosHydraulicStructureBoundaryCondition::Type::OutputLevel;
        else
          bc.type = ReosHydraulicStructureBoundaryCondition::Type::NotDefined;

        mSeries.append( bc );
      }

      while ( !stream.atEnd() )
      {
        const QString valueLine = stream.readLine().trimmed();
        const QStringList valueParts = valueLine.split( QRegularExpression( "\\s+" ), Qt::SkipEmptyParts );
        Q_ASSERT( valueParts.count() == seriesHeader.count() + 1 );
        bool ok = false;
        double time = valueParts.at( 0 ).toDouble( &ok );
        if ( !ok )
          continue;
        for ( int i = 1; i < valueParts.count(); ++i )
        {
          const QString &valueStr = valueParts.at( i );
          {
            bool ok;
            double value = valueStr.toDouble( &ok );
            if ( !ok )
              continue;
            mSeries[i - 1].time.append( time );
            mSeries[i - 1].value.append( value );
          }
        }
      }
    }
  }
}

ReosTelemacLiquidBoundaries::TelemacLiquidBoundary *ReosTelemacLiquidBoundaries::boundaryCondition( int rank, ReosHydraulicStructureBoundaryCondition::Type type ) const
{
  if ( mSeries.isEmpty() )
    parse();

  if ( mSeries.isEmpty() )
    return nullptr;

  for ( TelemacLiquidBoundary &bc : mSeries )
  {
    if ( bc.rank == rank && bc.type == type )
      return &bc;
  }

  return nullptr;
}
