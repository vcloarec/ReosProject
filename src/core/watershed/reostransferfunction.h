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

class ReosRunoff;
class ReosWatershed;

//! Class that represents a hydrograph
class ReosHydrograph : public ReosTimeSerieVariableTimeStep
{
  public:
    ReosHydrograph( QObject *parent = nullptr ): ReosTimeSerieVariableTimeStep( parent ) {}

    QString type() const override {return QStringLiteral( "runoff-hydrograph" );}
    QColor color() const override;
};

//! Class that reprsents a transfer function, that is a tranformation from runoff to hydrograph
class ReosTransferFunction : public ReosDataObject
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

  protected:
    void encodeBase( ReosEncodedElement &element ) const;
    ReosTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

  private:
    ReosWatershed *mWatershed = nullptr;
    ReosParameterDuration *mConcentrationTime = nullptr;
    ReosParameterArea *mArea = nullptr;
};


//! Abstract class that represents a transfer function factory
class ReosTransferFunctionFactory
{
  public:
    virtual QString type() const = 0;
    virtual QString displayText() const = 0;

    virtual ReosTransferFunction *createTransferFunction( ReosWatershed *watershed = nullptr ) const = 0;
    virtual ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const = 0;

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
class ReosTransferFunctionFactories : public ReosModule
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

  private:
    ReosTransferFunctionFactories( ReosModule *parent = nullptr );
    static ReosTransferFunctionFactories *sInstance;
    using Factory = std::unique_ptr<ReosTransferFunctionFactory>;
    ReosTransferFunctionFactoriesModel *mModel = nullptr;
    std::vector<Factory> mFactories;
};

//**********************************************************
//**********************************************************

class ReosTransferFunctionLinearReservoir : public ReosTransferFunction
{
    Q_OBJECT
  public:
    ReosTransferFunctionLinearReservoir( ReosWatershed *parent = nullptr );
    ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *hydrographParent = nullptr ) const override;
    QString type() const override {return QStringLiteral( "transfer-function-linear-reservoir" );}
    ReosEncodedElement encode() const override;

    static ReosTransferFunctionLinearReservoir *decode( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

    ReosParameterDouble *factorToLagTime() const;
    ReosParameterBoolean *useConcentrationTime() const;
    ReosParameterDuration *lagTime() const;

  private:
    ReosTransferFunctionLinearReservoir( const ReosEncodedElement &element, ReosWatershed *parent = nullptr );
    ReosParameterDuration *mLagTime;
    ReosParameterBoolean *mUseConcentrationTime;
    ReosParameterDouble *mFactorToLagTime;
};

class ReosTransferFunctionLinearReservoirFactory: public ReosTransferFunctionFactory
{
  public:
    QString type() const override {return QStringLiteral( "transfer-function-linear-reservoir" );}
    QString displayText() const override {return QObject::tr( "Linear reservoir" );}
    ReosTransferFunction *createTransferFunction( ReosWatershed *watershed ) const override;
    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const override;
};

//**********************************************************

class ReosTransferFunctionGeneralizedRationalMethod : public ReosTransferFunction
{
    Q_OBJECT
  public:
    ReosTransferFunctionGeneralizedRationalMethod( ReosWatershed *watershed = nullptr );
    QString type() const override {return QStringLiteral( "transfer-function-generalized-rational-method" );}
    ReosHydrograph *applyFunction( ReosRunoff *runoff, QObject *hydrographParent = nullptr ) const override;
    ReosEncodedElement encode() const override;

    static ReosTransferFunction *decode( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );

  private:
    ReosTransferFunctionGeneralizedRationalMethod( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr );
};

class ReosTransferFunctionGeneralizedRationalMethodFactory: public ReosTransferFunctionFactory
{
  public:
    QString type() const override {return QStringLiteral( "transfer-function-generalized-rational-method" );}
    QString displayText() const override {return QObject::tr( "Generalized Rational Method" );}
    ReosTransferFunction *createTransferFunction( ReosWatershed *watershed ) const override;
    ReosTransferFunction *createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed = nullptr ) const override;
};

//**********************************************************
#endif // REOSTRANSFERFUNCTION_H
