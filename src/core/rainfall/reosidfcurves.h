/***************************************************************************
  reosidfcurves.h - ReosIdfCurves

 ---------------------
 begin                : 2.2.2021
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
#ifndef REOSIDFCURVES_H
#define REOSIDFCURVES_H

#include <map>
#include <memory>

#include <QAbstractTableModel>
#include <QPointer>

#include "reosparameter.h"
#include "reosdataobject.h"
#include "reosmodule.h"

class ReosIntensityDurationInterval;

/**
 *  Class that contains double parameters needed by a formula that calculates rainfall height depending of its duration
 *  The parameters count depends of the formula
 */
class ReosIdfParameters: public QObject
{
    Q_OBJECT
  public:
    ReosIdfParameters( ReosIntensityDurationInterval *interval,
                       const QString  &formulaName,
                       const QStringList parameterNames,
                       ReosDuration::Unit parameterTimeUnit,
                       ReosDuration::Unit resultTimeUnit );

    //! Returns the parameters count
    int parametersCount();

    //! Returns the parameter at position \a i
    ReosParameterDouble *parameter( int i );

    //! Returns whether all the parameters are valid
    bool isValid() const;

    //! Returns the time unit used by the formula with these parameters, minutes by default
    ReosDuration::Unit parameterTimeUnit();

    //! Sets the time unit used by the formula with these parameters, minutes by default
    void setParameterTimeUnit( ReosDuration::Unit timeUnit );

    //! Returns the time unit used by the formula for the result, hous by default
    ReosDuration::Unit resutlTimeUnit();

    //! Sets the time unit used by the formula for the result, hour by default
    void setResultTimeUnit( ReosDuration::Unit timeUnit );

    //! Formula name retlated to parameters
    const QString formulaName;

    ReosEncodedElement encode() const;
    static void decode( const ReosEncodedElement &element, ReosIntensityDurationInterval *interval );

  signals:
    void changed();

  private:
    QVector<ReosParameterDouble *> mParameters;
    ReosDuration::Unit mParameterTimeUnit = ReosDuration::minute;
    ReosDuration::Unit mResultTimeUnit = ReosDuration::hour;

};

//! Class that represents an intensity duration formula
class REOSCORE_EXPORT ReosIdfFormula
{
  public:
    virtual ~ReosIdfFormula();

    //! Returns the name of the formula
    virtual QString name() const = 0;

    //! Returns the value (in mm) of the rainfall height corresponding at the \a duration with given \a parameters
    virtual double height( const ReosDuration &duration, ReosIdfParameters *parameters ) const = 0;

    //! Creates paramters and add it to the \a interval
    ReosIdfParameters *createParameters( ReosIntensityDurationInterval *interval, ReosDuration::Unit parameterTimeUnit, ReosDuration::Unit resultTimeUnit ) const;

    //! Returns thelist of the parameter names
    virtual QStringList parametersNames() const = 0;

    virtual QPixmap formulaImage() const;
};

//! Class that register rainfall intensity duration formula. It is a singleton class that can be called everywhere
class REOSCORE_EXPORT ReosIdfFormulaRegistery: public ReosModule
{
  public:
    //! Return a pointer to the formula with \a name, return nullptr if not exists
    ReosIdfFormula *formula( const QString &name );

    //! Register a formula, take ownership
    void registerFormula( ReosIdfFormula *formula );

    static void instantiate( ReosModule *parentModule );
    static ReosIdfFormulaRegistery *instance();

    //! Returns whether the static instance exist
    static bool isInstantiate();

    //! Returns the list of the names of formulas registered, those name are the key to retrieve the formula
    QStringList formulasList() const;

    QPixmap formulaImage( const QString name ) const;

  private:
    ReosIdfFormulaRegistery( ReosModule *parent = nullptr );

#ifdef _MSC_VER
    std::unique_ptr<ReosIdfFormula> dummy; // work arround for MSVC, if not =, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif
    std::map<QString, std::unique_ptr<ReosIdfFormula>> mFormulas;
    static ReosIdfFormulaRegistery *sIdfRegistery;
};

//! Montana Formula
class REOSCORE_EXPORT ReosIdfFormulaMontana: public ReosIdfFormula
{
  public:
    QString name() const override;
    double height( const ReosDuration &duration, ReosIdfParameters *parameters ) const override;
    QStringList parametersNames() const override;
    QPixmap formulaImage() const override;
};

//! Sherman Formula
class REOSCORE_EXPORT ReosIdfFormulaSherman: public ReosIdfFormula
{
  public:
    QString name() const override;
    double height( const ReosDuration &, ReosIdfParameters *parameters ) const override;
    QStringList parametersNames() const override;
    QPixmap formulaImage() const override;
};

/**
 * Class that represents an interval of a Intensity/duration curve.
 * An instance of this class can contain parameters for several formulas
 */
class ReosIntensityDurationInterval: public QObject
{
    Q_OBJECT
  public:
    ReosIntensityDurationInterval( const ReosDuration &start, const ReosDuration &end, QObject *parent );
    ReosIntensityDurationInterval( QObject *parent );

    //! Returns whether \a duration is continaes in the this interval
    bool isInInterval( const ReosDuration &duration ) const;

    //! Returns the parameters corresponding to the formula with \a formulaName
    ReosIdfParameters *parameters( const QString &formulaName ) const;

    //! Compare this interval with \a other, return true if this interval is before \a other
    bool operator<( const ReosIntensityDurationInterval &other ) const;
    //! Compare this interval with \a other, return true if this interval is after \a other
    bool operator>( const ReosIntensityDurationInterval &other ) const;
    //! Compare this interval with \a other, return true if this interval intersect \a other
    bool intersect( const ReosIntensityDurationInterval &other ) const;

    //! Returns the parameter corresponding to the start duration of this interval
    ReosParameterDuration *start() const {return mStartDuration;}
    //! Returns the parameter corresponding to the end duration of this interval
    ReosParameterDuration *end() const {return mEndDuration;}

    //! Returns the value duration corresponding to the start of this interval
    ReosDuration startDuration() const;

    //! Returns the value duration corresponding to the end of this interval
    ReosDuration endDuration() const;

    //! add the parameters to the interval
    void addParameters( ReosIdfParameters *parameters );

    ReosEncodedElement encode() const;
    static ReosIntensityDurationInterval *decode( const ReosEncodedElement &element, QObject *parent );

  signals:
    void changed();

  private:

    ReosParameterDuration *mStartDuration = nullptr;
    ReosParameterDuration *mEndDuration = nullptr;

    QMap<QString, ReosIdfParameters *> mParameters;
};

/**
 * Class that represents a intensity duration curve.
 * An instance of this class can represent several formulas
 */
class REOSCORE_EXPORT ReosIntensityDurationCurve: public ReosDataObject
{
    Q_OBJECT
  public:
    ReosIntensityDurationCurve( const ReosDuration &returnPeriod, QObject *parent = nullptr );
    ReosIntensityDurationCurve( QObject *parent = nullptr );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosDataObject::staticType() + ':' +  QStringLiteral( "intensity-duration-curve" );}

    //! Returns the parametes corresponding to the return period of this curve
    ReosParameterDuration *returnPeriod() const;

    //! Sets the current formula name that will be used to return height/intensity rainfall
    void setCurrentFormula( const QString &formulaName );

    //! Returns the current formula name that will be used to return height/intensity rainfall
    QString currentFormula() const {return mCurrentFormulaName;}

    //! Mounts the formula from the \a registery
    void setupFormula( ReosIdfFormulaRegistery *registery );

    //! Return whether the current formula is valid
    bool isFormulaValid() const {return mCurrentFormula != nullptr;}

    //! Returns the interval count in this curve
    int intervalCount() const {return mIntensityDurationIntervals.count();}

    /**
     * Adds an interval to this curve, returns false if interval can't be added (new one intersects existent one)
     */
    bool addInterval( const ReosDuration &start, const ReosDuration &end );

    //! Sets duration extremity value for the interval at position \i, becareful do not handle new eventual order of interval
    bool setIntervalValue( int i, const ReosDuration &start, const ReosDuration &end );

    //! remove interval at positon \a i
    void removeInterval( int i );

    //! Returns the extremity interval duration
    QPair<ReosDuration, ReosDuration> timeInterval( int i ) const;

    //! Returns the calculated height with the current formula, return -1 for invalid value
    double height( const ReosDuration &duration, bool interpolationAllowed = false ) const;
    //! Returns the calculated height corresponding to the begining of the interval at position \a intervalIndex
    double firstHeight( int intervalIndex ) const;
    //! Returns the calculated height corresponding to the and of the interval at position \a intervalIndex
    double lastHeight( int intervalIndex ) const;

    //! Returns the calculated intensity with the current formula, return -1 for invalid value
    double intensity( const ReosDuration &duration, ReosDuration::Unit unit = ReosDuration::hour ) const;
    //! Returns the calculated intensity corresponding to the begining of the interval at position \a intervalIndex
    double firstIntensity( int intervalIndex, ReosDuration::Unit unit = ReosDuration::hour ) const;
    //! Returns the calculated intensity corresponding to the and of the interval at position \a intervalIndex
    double lastIntensity( int intervalIndex, ReosDuration::Unit unit = ReosDuration::hour ) const;

    //! Sets parameters for the interval at position \a i and for the \a formula
    ReosIdfParameters *createParameters( int i, ReosIdfFormula *formula,
                                         ReosDuration::Unit parameterTimeUnit = ReosDuration::minute,
                                         ReosDuration::Unit resultTimeUnit = ReosDuration::hour );

    //! Returns the parameters for interval at position \a i and for the current formula
    ReosIdfParameters *currentParameters( int i );

    //! Returns whether interval at position \a i is valid
    bool intervalIsValid( int i );

    //! Returns the intensity extent of the curve with unit time \a timeUnit
    QRectF extent( ReosDuration::Unit timeUnit ) const;

    //! Sets the time unit corresponding to parameter with current formula
    void setCurrentParameterTimeUnit( ReosDuration::Unit timeUnit );

    //! Returns the time unit corresponding to parameter with current formula, return minute by default
    ReosDuration::Unit currentParameterTimeUnit();

    //! Sets the time unit corresponding to result (if intensity result) with current formula
    void setCurrentResultTimeUnit( ReosDuration::Unit timeUnit );

    //! Returns the time unit corresponding to result (if intensity result) with current formula, return minute by default
    ReosDuration::Unit currentResultTimeUnit();

    ReosEncodedElement encode() const;
    static ReosIntensityDurationCurve *decode( const ReosEncodedElement &element, QObject *parent );

  signals:
    void intervalWillBeAdded( int pos );
    void intervalAdded();
    void intervalWillBeRemoved( int pos );
    void intervalRemoved();

  private:
    QString mCurrentFormulaName;
    ReosParameterDuration *mReturnPeriod = nullptr;
    ReosIdfFormula *mCurrentFormula = nullptr;
    QVector<ReosIntensityDurationInterval *> mIntensityDurationIntervals;
    QMap<QString, ReosDuration::Unit> mParametersTimesUnit;
    QMap<QString, ReosDuration::Unit> mResultTimesUnit;

    const ReosIntensityDurationInterval *interval( const ReosDuration &duration ) const;
    mutable int mLastDurationPos = 0;
};

/**
 * Model representing a intensity/duration curve
 */
class REOSCORE_EXPORT ReosIntensityDurationCurveTableModel:  public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosIntensityDurationCurveTableModel( ReosIntensityDurationCurve *curve, QObject *parent );

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override {return QModelIndex();}
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    //! return the curve defined by the model
    ReosIntensityDurationCurve *curve() const;

  public slots:
    void setCurrentFormula( const QString &formulaName );

  private slots:
    void onIntervalWillBeAdded( int pos );
    void onIntervalAdded();
    void onIntervalWillBeRemoved( int pos );
    void onIntervaleRemoved();

  private:
    ReosIntensityDurationCurve *mCurve = nullptr;
};

/**
 * Class that represents several intensity/duration curves with different return periiod. This class representes complete IDF concept.
 */
class REOSCORE_EXPORT ReosIntensityDurationFrequencyCurves : public ReosDataObject
{
  public:
    ReosIntensityDurationFrequencyCurves( QObject *parent = nullptr ): ReosDataObject( parent )
    {}

    QString type() const override {return staticType();}
    static QString staticType() {return ReosDataObject::staticType() + ':' +  QStringLiteral( "intensity-duration-curves" );}

    //! Adds an intensity-duration curve
    void addCurve( ReosIntensityDurationCurve *curve, QString name );

    //! Returns the count of curve
    int curvesCount();

    //! Returns the curve at position \a i
    ReosIntensityDurationCurve *curve( int i );

    //! Returns the name of the curve at position \a i
    QString name( int i );

    //! Returns the intensity extent of all the curves (duration in minutes and intensity in mm/h)
    QRectF fullExtent() const;

  private:
    QList<QPointer<ReosIntensityDurationCurve>> mCurves;
    QStringList mNames;
    ReosDuration::Unit mTimeUnit = ReosDuration::minute;
};

#endif // REOSIDFCURVES_H
