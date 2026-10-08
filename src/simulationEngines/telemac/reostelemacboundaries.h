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

#ifndef REOSTELEMACBOUNDARIES_H
#define REOSTELEMACBOUNDARIES_H

#include <QPolygonF>
#include "reoshydraulicstructureboundarycondition.h"
#include "reostimeseries.h"

class ReosMesh;

class ReosTelemacBoundaries
{
  public:
    ReosTelemacBoundaries() = default;
    ReosTelemacBoundaries( ReosMesh *mesh, const QString &boudaryFilePath = QString() );

    int boundaryVertexCount() const;
    QList<int> boundaryVertexIndexes() const;
    QPolygonF envelop() const;

    QList<ReosHydraulicStructureBoundaryCondition::Type> boundaryConditionTypes() const;
    QList<QSet<int>> liquidDomainSegmentIndex() const;

  private:
    struct TelemacBoundaryLine
    {
        int LIHBOR = 2;
        int LIUBOR = 2;
        int LIVBOR = 2;

        int LITBOR = 2;

        int vertIndex;

        bool isSolidBoundary() const { return LIHBOR == 2 && LIUBOR == 2 && LIVBOR == 2; }
        bool isSameBoundary( const TelemacBoundaryLine &other ) const { return LIHBOR == other.LIHBOR && LIUBOR == other.LIUBOR && LIVBOR == other.LIVBOR && LITBOR == other.LITBOR; }

        ReosHydraulicStructureBoundaryCondition::Type boundaryConditionType() const
        {
          if ( LIHBOR == 4 && LIUBOR == 5 && LIVBOR == 5 )
            return ReosHydraulicStructureBoundaryCondition::Type::InputFlow;
          else if ( LIHBOR == 5 && LIUBOR == 4 && LIVBOR == 4 )
            return ReosHydraulicStructureBoundaryCondition::Type::OutputLevel;
          else
            return ReosHydraulicStructureBoundaryCondition::Type::NotDefined;
        }
    };

    ReosMesh *mMesh = nullptr;
    QString mBoundaryFilePath;
    QPolygonF mEnvelop;
    QList<TelemacBoundaryLine> mTelemacBoundaryVertex;
    QList<ReosHydraulicStructureBoundaryCondition::Type> mBoundaryConditionTypes;
    QList<QSet<int>> mLiquidDomainSegmentIndex;

    void populateTelemacBoundaryVertexFromFile();
    void populateEnvelopFromTelemacBoundaryVertex();
};


class ReosTelemacLiquidBoundaries
{
  public:
    struct TelemacLiquidBoundary
    {
        int rank = -1;
        QString header;
        QString unit;
        QList<double> time;
        QList<double> value;
        ReosHydraulicStructureBoundaryCondition::Type type;
    };

    ReosTelemacLiquidBoundaries() = default;
    ReosTelemacLiquidBoundaries( const QString &liquidBoudaryFilePath );


    TelemacLiquidBoundary *boundaryCondition( int rank, ReosHydraulicStructureBoundaryCondition::Type type ) const;

  private:
    QString mLiquidBoundaryFilePath;
    mutable QList<TelemacLiquidBoundary> mSeries;

    void parse() const;
};

#endif // REOSTELEMACBOUNDARIES_H
