/***************************************************************************
  reoshydraulicsimulation.h - ReosHydraulicSimulation

 ---------------------
 begin                : 19.3.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICSIMULATION_H
#define REOSHYDRAULICSIMULATION_H

#include <QObject>

class ReosHydraulicStructure2D;
class ReosParameterDateTime;
class ReosParameterDuration;

class ReosHydraulicSimulation : public QObject
{
    Q_OBJECT
  public:
    ReosHydraulicSimulation( QObject *parent = nullptr );

    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure );
    void createSelafinInputGeometry( ReosHydraulicStructure2D *hydraulicStructure );
    void createBoundaryConditionFiles( ReosHydraulicStructure2D *hydraulicStructure );


    void createSteeringFile( ReosHydraulicStructure2D *hydraulicStructure );

    ReosParameterDateTime *startTime() const;
    ReosParameterDateTime *endTime() const;
    ReosParameterDuration *timeStep() const;

  private:
    ReosParameterDateTime *mStartTime;
    ReosParameterDateTime *mEndTime;
    ReosParameterDuration *mTimeStep;


    QString mDirName = QStringLiteral( "TELEMAC_simulation" );
    QString mGeomFileName = QStringLiteral( "geom_input.slf" );
    QString mResultFileName = QStringLiteral( "result.slf" );
    QString mBoundaryFileName = QStringLiteral( "boundary.bc" );
    QString mBoundaryConditionFileName = QStringLiteral( "boundaryCondition.sql" );
    QString mSteeringFileName = QStringLiteral( "simulation.cas" );


};


#endif // REOSHYDRAULICSIMULATION_H
