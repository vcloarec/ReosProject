/***************************************************************************
  reosrunoffmodel.h - ReosRunoffModel

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
#ifndef REOSRUNOFFMODEL_H
#define REOSRUNOFFMODEL_H

#include <QPointer>

#include "reosdataobject.h"
#include "reosduration.h"
#include "reosparameter.h"
#include "reostimeserie.h"

class Runoff;

/**
 * Abstract class that represent a runoff calculation on a rainfall
 */
class ReosRunoffModel : public QObject
{
    Q_OBJECT
  public:
    ReosRunoffModel();
    virtual ~ReosRunoffModel() = default;

    //! Returns the runoff type
    virtual QString runoffType() const = 0;

    //! Returns the list of parameters
    virtual QList<ReosParameter *> parameters() const = 0;

    //! Returns the name of this runoff model
    ReosParameterString *name() const;

    //! Applies the model on the \a rainfall and put the result in runoffResult
    virtual bool applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, QVector<double> &runoffResult ) = 0;

  signals:
    void modelChanged();
  protected:
    void connectParameters();

  private:
    ReosParameterString *mName;
};

class ReosRunoffConstantCoefficientModel: public ReosRunoffModel
{
  public:
    ReosRunoffConstantCoefficientModel();

    QString runoffType() const override {return QStringLiteral( "constant-coefficient-runoff" );}
    QList<ReosParameter *> parameters() const override;
    bool applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, QVector<double> &runoffResult ) override;

    //! Returns the constant coefficient parameter
    ReosParameterDouble *coefficient();

  private:
    ReosParameterDouble *mCoefficient;
};

//! Abstract class that represents the application of a runoff model on a rainfall
class ReosRunoff : public ReosDataObject
{
    Q_OBJECT
  public:
    //! Constructor with the \a runoffModel and the ·\a rainfall
    ReosRunoff( ReosRunoffModel *runoffModel, ReosTimeSerieConstantInterval *rainfall, QObject *parent = nullptr );
    ~ReosRunoff() = default;

    QString type() const override {return QStringLiteral( "runoff" );}

    //! Returns the current values count
    int valueCount() const;

    //! Returns the time step of the runoff data, that is the time step of the rainfall
    ReosDuration timeStep() const;

    //! Returns the value at positon \i
    double value( int i ) const;

  public slots:
    //! Updates the values
    bool updateValues();

  protected:
    QPointer<ReosTimeSerieConstantInterval> mRainfall;
    QPointer<ReosRunoffModel> mRunoffModel;
    QVector<double> mData;


};


#endif // REOSRUNOFFMODEL_H
