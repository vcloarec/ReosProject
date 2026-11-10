/***************************************************************************
  reostelemacsteeringfile.h - ReosTelemacSteeringFile

 ---------------------
 begin                : 2.10.2026
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

#ifndef REOSTELEMACSTEERINGFILE_H
#define REOSTELEMACSTEERINGFILE_H

#pragma once

#include <QDateTime>
#include <QString>
#include <QMap>
#include <QObject>
#include <memory.h>

#include "reosduration.h"
#include "reostelemac.h"
#include "reostelemac2dinitialcondition.h"

class ReosTelemacSteeringFile
{
  public:
    ReosTelemacSteeringFile() = default;
    ReosTelemacSteeringFile( const QString &path );
    void open();
    void save( const QString &path = QString() );

    const QString &path() const;

    void setKey( const QString &key, const QString &value );
    QString value( const QString &key ) const;
    void addComment( const QString &comment );

    bool hasKey( const QString &key ) const;

    int keyCount() const;
    int lineCount() const;

    bool isValid() const;

    QString geomFileName() const;
    QString geomFilePath() const;

    QString resultFileName() const;
    QString boundaryFileName() const;
    QString boundaryLiquidFileName() const;

    const QDateTime referenceTime() const;
    const ReosDuration duration() const;
    const ReosDuration timeStep() const;

    const QList<double> prescribedFlowRate() const;
    const QList<double> prescibedElevation() const;

    ReosTelemac::Equation equation() const;

    double courantNumber() const;

    int outputPeriodResult2D() const;
    int outputPeriodResultHydrograph() const;

    ReosTelemac2DInitialCondition::Type initialConditionType() const;

  private:
    class SteeringLine
    {
      public:
        SteeringLine( const QString &line );
        SteeringLine( const QString &key, const QString &value );

        QString key() const;
        QString value() const;

        void setValue( const QString &value );

      private:
        QString mKey;
        QString mValue;
    };

    QString mPath;
    std::vector<std::unique_ptr<SteeringLine>> mLines;
    QMap<QString, SteeringLine *> mLinesMap;
};




#endif // REOSTELEMACSTEERINGFILE_H
