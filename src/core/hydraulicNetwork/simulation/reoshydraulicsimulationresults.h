/***************************************************************************
  reoshydraulicsimulationresults.h - ReosHydraulicSimulationResults

 ---------------------
 begin                : 1.4.2022
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
#ifndef REOSHYDRAULICSIMULATIONRESULTS_H
#define REOSHYDRAULICSIMULATIONRESULTS_H

#include <QObject>
#include <QVector>

#include "reosmeshdatasetsource.h"

class ReosHydraulicSimulationResults : public ReosMeshDatasetSource
{
    Q_OBJECT
  public:
    enum class DatasetType
    {
      WaterLevel,
      WaterDepth,
      Velocity,
    };

    ReosHydraulicSimulationResults( QObject *parent = nullptr );

    QString groupName( int groupIndex ) const override;
    bool groupIsScalar( int groupIndex ) const override;
    virtual DatasetType datasetType( int groupIndex ) const = 0;

};

#endif // REOSHYDRAULICSIMULATIONRESULTS_H
