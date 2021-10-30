/***************************************************************************
  reostransferfunction.h - ReosTransferFunction

 ---------------------
 begin                : 19.2.2021
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
#ifndef REOSTRANSFERFUNCTION_H
#define REOSTRANSFERFUNCTION_H

#include "reostimeserie.h"
#include "reosmodule.h"
#include "reosencodedelement.h"
#include "reosprocess.h"
#include "reoshydrograph.h"

class ReosRunoff;
class ReosWatershed;

//! Process abstract class that handle the calculation of the hydrograph an onother thread
class REOSCORE_EXPORT ReosTransferFunctionCalculation : public ReosProcess
{
  public:
    //! Returns the hydrograph result, has to be call after the process is finished, if \a parent is not specified, the caller need to take ownership
    ReosHydrograph *hydrograph( QObject *parent = nullptr );

  protected:
    std::unique_ptr<ReosHydrograph> mHydrograph;
};

//! Class that reprsents a transfer function, that is a tranformation from runoff to hydrograph
class REOSCORE_EXPORT ReosTransferFunction : public ReosDataObject
{
  public:
    ReosTransferFunction( ReosWatershed *watershed = nullptr );

    //! Returns an hydrograph from the \a runoff
    virtual ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *hydrographParent = nullptr ) const = 0;

    /**
     *  Returns the concentration time parameter of the transform function, it is the concentration time of the watershed if not null.
     *  If the watershhed is null, it a independant parameter
     */
    ReosParameterDuration *concentrationTime() const;

    /**
     *  Returns the area parameter of the transform function, it is the area of the watershed if not null.
     *  If the watershhed is null, it a independant parameter
     */
    ReosParameterArea *area() const;

    virtual ReosEncodedElement encode() const = 0;

    //! Returns a calculaton process for hydrograph calculation, the caller take ownership
    virtual ReosTransferFunctionCalculation *calculationProcess( ReosRunoff *runoff ) = 0;

  protected:
    void encodeBase( ReosEncodedElement &element ) const;
    ReosTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

  private:
    ReosWatershed *mWatershed = nullptr;
    ReosParameterDuration *mConcentrationTime = nullptr;
    ReosParameterArea *mArea = nullptr;
};


//! Abstract class that represents a transfer function factory
class REOSCORE_EXPORT ReosTransferFunctionFactory
{
  public:
    virtual QString type() const = 0;
    virtual QString displayText() const = 0;

    virtual ReosTransferFunction *createTransferFunction( ReosWatershed *watershed = nullptr ) const = 0;
    virtual ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const = 0;

    //! Returns a presentation text of the transfer function
    virtual QString presentationText() const {return QString();};
    //! Returns a image that represent the formulation of the transfer
    virtual QPixmap formulation() const {return QPixmap();}
    //! Returns the variable descritpion of the transfer function
    virtual QString variablesDescription() const {return QString();}

};

//! List model class that is used to display transfer function list
class ReosTransferFunctionFactoriesModel : public QAbstractListModel
{
  public:
    ReosTransferFunctionFactoriesModel( std::vector<std::unique_ptr<ReosTransferFunctionFactory>> &factories, QObject *parent );

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

  private:
    using Factory = std::unique_ptr<ReosTransferFunctionFactory>;
    std::vector<Factory> &mFactories;

};

//! Singleton class used to store and to acces to transfer function factory
class REOSCORE_EXPORT ReosTransferFunctionFactories : public ReosModule
{
  public:
    static void instantiate( ReosModule *parent );
    static bool isInstantiate();
    static ReosTransferFunctionFactories *instance();

    //! Creates a transfer fuction of type \a type linked to \a watershed, return nullptr if \a type is not handled
    ReosTransferFunction *createTransferFunction( const QString &type, ReosWatershed *watershed );

    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &elem, ReosWatershed *watershed );

    //! Adds a factory
    void addFactory( ReosTransferFunctionFactory *factory );

    //! Return the type of the factory wih index \a i
    QString type( int i );

    //! Returns the facory counr
    int factoryCount() const;

    //! Returns the index of the factory with \a type, return -1 if \a is not handled
    int index( const QString &type ) const;

    //! Returns a list model to display the list of factory
    QAbstractListModel *listModel() const;

    //! Returns a presentation text of the transfer function of \a type
    QString presentationText( const QString &type ) const;

    //! Returns a image that represent the formulation of the transfer function \a type
    QPixmap formulation( const QString &type ) const;

    //! Returns the variable descritpion of the transfer function \a type
    QString variablesDescription( const QString &type ) const;

  private:
    ReosTransferFunctionFactories( ReosModule *parent = nullptr );
    static ReosTransferFunctionFactories *sInstance;
    using Factory = std::unique_ptr<ReosTransferFunctionFactory>;
    ReosTransferFunctionFactoriesModel *mModel = nullptr;
    std::vector<Factory> mFactories;
};

//**********************************************************
//**********************************************************

class REOSCORE_EXPORT ReosTransferFunctionLinearReservoir : public ReosTransferFunction
{
    Q_OBJECT
  public:
    ReosTransferFunctionLinearReservoir( ReosWatershed *parent = nullptr );
    ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *hydrographParent = nullptr ) const override;
    QString type() const override {return QStringLiteral( "transfer-function-linear-reservoir" );}
    ReosEncodedElement encode() const override;
    virtual ReosTransferFunctionCalculation *calculationProcess( ReosRunoff *runoff ) override;

    static ReosTransferFunctionLinearReservoir *decode( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    ReosParameterDouble *factorToLagTime() const;
    ReosParameterBoolean *useConcentrationTime() const;
    ReosParameterDuration *lagTime() const;

  private:
    ReosTransferFunctionLinearReservoir( const ReosEncodedElement &element, ReosWatershed *parent = nullptr );
    ReosParameterDuration *mLagTime = nullptr;
    ReosParameterBoolean *mUseConcentrationTime = nullptr;
    ReosParameterDouble *mFactorToLagTime = nullptr;

    class Calculation: public ReosTransferFunctionCalculation
    {
      public:
        Calculation( const QVector<double> runoffData,
                     ReosDuration  lagTime,
                     const ReosArea &area,
                     const ReosDuration &timeStep,
                     const QDateTime &referenceTime );

        void start() override;

      private:
        QVector<double> mRunoffData;
        ReosDuration mLagTime;
        ReosArea mArea;
        ReosDuration mTimeStep;
        QDateTime mReferenceTime;
    };

  private:
    std::unique_ptr<ReosHydrograph> mHydrograph;
    QVector<double> mRunoffData;
    int mReduceTimeStepFactor;
    ReosDuration mTimeStep;
    QDateTime mReferenceTime;
    ReosDuration mPeakTime;
    double mPeakFactor;
    ReosArea mArea;


};

class REOSCORE_EXPORT ReosTransferFunctionLinearReservoirFactory: public ReosTransferFunctionFactory
{
  public:
    QString type() const override {return QStringLiteral( "transfer-function-linear-reservoir" );}
    QString displayText() const override {return QObject::tr( "Linear reservoir" );}
    ReosTransferFunction *createTransferFunction( ReosWatershed *watershed ) const override;
    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const override;
    QString presentationText() const override;;
    QPixmap formulation() const override;
    QString variablesDescription() const override;
};

//**********************************************************

class REOSCORE_EXPORT ReosTransferFunctionGeneralizedRationalMethod : public ReosTransferFunction
{
    Q_OBJECT
  public:
    ReosTransferFunctionGeneralizedRationalMethod( ReosWatershed *watershed = nullptr );
    QString type() const override {return QStringLiteral( "transfer-function-generalized-rational-method" );}
    ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *hydrographParent = nullptr ) const override;
    ReosEncodedElement encode() const override;
    ReosTransferFunctionCalculation *calculationProcess( ReosRunoff *runoff ) override;

    static ReosTransferFunction *decode( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

  private:
    ReosTransferFunctionGeneralizedRationalMethod( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    class Calculation: public ReosTransferFunctionCalculation
    {
      public:
        Calculation( const QVector<double> runoffData,
                     const ReosDuration &concentrationTime,
                     const ReosArea &area,
                     const ReosDuration &timeStep,
                     const QDateTime &referenceTime );

        void start() override;

      private:
        QVector<double> mRunoffData;
        ReosDuration mConcentrationTime;
        ReosArea mArea;
        ReosDuration mTimeStep;
        QDateTime mReferenceTime;
    };
};

class REOSCORE_EXPORT ReosTransferFunctionGeneralizedRationalMethodFactory: public ReosTransferFunctionFactory
{
  public:
    QString type() const override {return QStringLiteral( "transfer-function-generalized-rational-method" );}
    QString displayText() const override {return QObject::tr( "Generalized Rational Method" );}
    ReosTransferFunction *createTransferFunction( ReosWatershed *watershed ) const override;
    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const override;
    QString presentationText() const override;;
    QPixmap formulation() const override;
    QString variablesDescription() const override;
};

//**********************************************************

//**********************************************************

class REOSCORE_EXPORT ReosTransferFunctionSCSUnitHydrograph : public ReosTransferFunction
{
    Q_OBJECT
  public:
    ReosTransferFunctionSCSUnitHydrograph( ReosWatershed *watershed = nullptr );
    QString type() const override {return QStringLiteral( "transfer-function-scs-unit-hydrograph" );}
    ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *parent = nullptr ) const override;
    ReosEncodedElement encode() const override;
    ReosTransferFunctionCalculation *calculationProcess( ReosRunoff *runoff ) override;

    static ReosTransferFunction *decode( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    ReosParameterDouble *peakRateFactor() const;
    ReosParameterDouble *factorToLagTime() const;
    ReosParameterBoolean *useConcentrationTime() const;
    ReosParameterDuration *lagTime() const;

    struct UH_SCS_dimensionneless
    {
      int peakRateFactor;
      double dimensionlessTimeStep;
      QVector<double> dimensionlessRate;
    };

  private:
    ReosTransferFunctionSCSUnitHydrograph( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    class Calculation: public ReosTransferFunctionCalculation
    {
      public:
        Calculation( const QVector<double> runoffData,
                     int reduceTimeStepFactor,
                     const ReosDuration &timeStep,
                     const QDateTime &referenceTime,
                     const ReosDuration &peakTime,
                     double peakFactor,
                     const ReosArea &area );

        void start() override;

      private:
        QVector<double> mRunoffData;
        int mReduceTimeStepFactor = 0;
        ReosDuration mTimeStep;
        QDateTime mReferenceTime;
        ReosDuration mPeakTime;
        double mPeakFactor = 0;
        ReosArea mArea;

        std::unique_ptr<ReosHydrograph> createUnitHydrograph() const;
    };

    ReosParameterDouble *mPeakRateFactor = nullptr;
    ReosParameterDuration *mLagTime = nullptr;
    ReosParameterBoolean *mUseConcentrationTime = nullptr;
    ReosParameterDouble *mFactorToLagTime = nullptr;

    static QVector<UH_SCS_dimensionneless> sTable;

    // returns the index in the table of the peak factor just greater or equal
    static int indexInTable( double peakRateFactor, bool &exact );

};

class REOSCORE_EXPORT ReosTransferFunctionSCSUnitHydrographFactory: public ReosTransferFunctionFactory
{
  public:
    QString type() const override {return QStringLiteral( "transfer-function-scs-unit-hydrograph" );}
    QString displayText() const override {return QObject::tr( "SCS Unit Hydrograph" );}
    ReosTransferFunction *createTransferFunction( ReosWatershed *watershed ) const override;
    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const override;
    QString presentationText() const override;;
    QPixmap formulation() const override;
    QString variablesDescription() const override;
};

//**********************************************************

//**********************************************************

class REOSCORE_EXPORT ReosTransferFunctionNashUnitHydrograph : public ReosTransferFunction
{
    Q_OBJECT
  public:
    ReosTransferFunctionNashUnitHydrograph( ReosWatershed *watershed = nullptr );
    QString type() const override {return QStringLiteral( "transfer-function-nash-unit-hydrograph" );}
    ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *parent = nullptr ) const override;
    ReosEncodedElement encode() const;

    static ReosTransferFunction *decode( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    ReosParameterDuration *KParam() const;
    ReosParameterInteger *nParam() const;
    ReosParameterBoolean *useConcentrationTime() const;

    ReosTransferFunctionCalculation *calculationProcess( ReosRunoff *runoff ) override;

  private:
    ReosTransferFunctionNashUnitHydrograph( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    class Calculation: public ReosTransferFunctionCalculation
    {
      public:
        Calculation( const QVector<double> runoffData,
                     const ReosDuration &timeStep,
                     const QDateTime &referenceTime,
                     const ReosDuration KParam,
                     int nParam,
                     const ReosArea &area );

        void start() override;

      private:
        QVector<double> mRunoffData;
        const ReosDuration mKParam;
        int mNParam;
        QDateTime mReferenceTime;
        ReosArea mArea;
        const ReosDuration mTimeStep;

        std::unique_ptr<ReosHydrograph> createUnitHydrograph( const ReosDuration &timeStep ) const;
    };


    ReosParameterDuration *mKParam = nullptr;
    ReosParameterInteger *mNParam = nullptr;
    ReosParameterBoolean *mUseConcentrationTime = nullptr;
};

class REOSCORE_EXPORT ReosTransferFunctionNashUnitHydrographFactory: public ReosTransferFunctionFactory
{
  public:
    QString type() const override {return QStringLiteral( "transfer-function-nash-unit-hydrograph" );}
    QString displayText() const override {return QObject::tr( "Nash Unit Hydrograph" );}
    ReosTransferFunction *createTransferFunction( ReosWatershed *watershed ) const override;
    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const override;
    QString presentationText() const override;;
    QPixmap formulation() const override;
    QString variablesDescription() const override;
};

//**********************************************************



#endif // REOSTRANSFERFUNCTION_H
