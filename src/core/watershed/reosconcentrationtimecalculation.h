/***************************************************************************
  reosconcentrationtimecalculation.h - ReosConcentrationTimeCalculation

 ---------------------
 begin                : 13.2.2021
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
#ifndef REOSCONCENTRATIONTIMECALCULATION_H
#define REOSCONCENTRATIONTIMECALCULATION_H

#define SIP_NO_FILE

#include <map>
#include <memory>

#include <QColor>
#include <QAbstractTableModel>
#include <QPixmap>
#include <QStringList>

#include "reoscore.h"
#include "reosparameter.h"

/**
 * Abstract class that represent en watersehd concentration time calculation
 */
class REOSCORE_EXPORT ReosConcentrationTimeFormula
{
  public:
    ~ReosConcentrationTimeFormula();

    //! Paramaters for the concentration time calculation
    struct Parameters
    {
      double slope = 0; //!< in meters/meters
      ReosArea area = ReosArea();
      double drop = 0; //!< in meters
      double length = 0; //!< in meters
      double relativeAverageElevation = 0; //!< in meters
    };

    //! Returns the name if the formula
    virtual QString name() const = 0;

    //! Returns whether all the \a parameters ar in the validity domain of the formula
    virtual bool isInValidityDomain( const Parameters &parameters ) const = 0;

    //! Returns the value of the concentration time
    virtual ReosDuration concentrationTime( const Parameters &parameters ) const = 0;

    //! Returns whether the calculation can be executed (even if the parameters are outside the validity domain)
    virtual bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const = 0;

    //! Returns an image with the formula, parameters have to be in S.I. unit and result on minutes
    virtual QPixmap formulaImage() const = 0;
};

//! Singleton class that is used to store concentration time formula
class REOSCORE_EXPORT ReosConcentrationTimeFormulasRegistery
{
  public:
    ~ReosConcentrationTimeFormulasRegistery();

    //! Returns the formulas that has the \a name
    ReosConcentrationTimeFormula *formula( const QString &name ) const;

    //! Returns a list of all registered formula names
    QStringList formulasList() const;

    //! Returns the count of registered formulas
    int formulasCount() const;

    //! Returns if the singleton is instantiate
    static bool isInstantiate();

    //! Returns the singleton instance
    static ReosConcentrationTimeFormulasRegistery *instance();

    //! Returns the instance of the singleton
    void registerFormulas( ReosConcentrationTimeFormula *formula );

  private:
    ReosConcentrationTimeFormulasRegistery();

#ifdef _MSC_VER
    std::unique_ptr<ReosConcentrationTimeFormula> dummy; // work arround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif

    std::map<QString, std::unique_ptr<ReosConcentrationTimeFormula>> mFormulas;
    static ReosConcentrationTimeFormulasRegistery *sInstance;
};

//********************************************************************

class REOSCORE_EXPORT ReosConcentrationTimeFormulaKirpich : public ReosConcentrationTimeFormula
{
  public:
    QString name() const override {return QStringLiteral( "Kirpich" );}
    ReosDuration concentrationTime( const Parameters &parameters ) const override;
    bool isInValidityDomain( const Parameters &parameters ) const override;
    bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const override;

    virtual QPixmap formulaImage() const override {return QPixmap( QStringLiteral( ":/formulas/concentrationTimeKirpich.svg" ) );}
};

class REOSCORE_EXPORT ReosConcentrationTimeFormulaPassini : public ReosConcentrationTimeFormula
{
  public:
    QString name() const override {return QStringLiteral( "Passini" );}
    ReosDuration concentrationTime( const Parameters &parameters ) const override;
    bool isInValidityDomain( const Parameters &parameters ) const override;
    bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const override;

    virtual QPixmap formulaImage() const override {return QPixmap( QStringLiteral( ":/formulas/concentrationTimePassini.svg" ) );}
};

class REOSCORE_EXPORT ReosConcentrationTimeFormulaVentura : public ReosConcentrationTimeFormula
{
  public:
    QString name() const override {return QStringLiteral( "Ventura" );}
    ReosDuration concentrationTime( const Parameters &parameters ) const override;
    bool isInValidityDomain( const Parameters & ) const override;
    bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const override;

    virtual QPixmap formulaImage() const override {return QPixmap( QStringLiteral( ":/formulas/concentrationTimeVentura.svg" ) );}
};

class REOSCORE_EXPORT ReosConcentrationTimeFormulaVenTeChow : public ReosConcentrationTimeFormula
{
  public:
    QString name() const override {return QStringLiteral( "Ven te Chow" );}
    ReosDuration concentrationTime( const Parameters &parameters ) const override;
    bool isInValidityDomain( const Parameters & ) const override;
    bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const override;

    virtual QPixmap formulaImage() const override {return QPixmap( QStringLiteral( ":/formulas/concentrationTimeVenTeChow.svg" ) );}
};

class REOSCORE_EXPORT ReosConcentrationTimeFormulaJohnstone : public ReosConcentrationTimeFormula
{
  public:
    QString name() const override {return QStringLiteral( "Johnstone" );}
    ReosDuration concentrationTime( const Parameters &parameters ) const override;
    bool isInValidityDomain( const Parameters &parameters ) const override;
    bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const override;

    virtual QPixmap formulaImage() const override {return QPixmap( QStringLiteral( ":/formulas/concentrationTimeJohnstone.svg" ) );}
};

class REOSCORE_EXPORT ReosConcentrationTimeFormulaGiandotti : public ReosConcentrationTimeFormula
{
  public:
    QString name() const override {return QStringLiteral( "Giandotti" );}
    ReosDuration concentrationTime( const Parameters &parameters ) const override;
    bool isInValidityDomain( const Parameters &parameters ) const override;
    bool canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const override;

    virtual QPixmap formulaImage() const override {return QPixmap( QStringLiteral( ":/formulas/concentrationTimeGiandotti.svg" ) );}
};

//********************************************************************

/**
 *  Class that represents the calculation of a concentration time onsidering severals formula
 */
class REOSCORE_EXPORT ReosConcentrationTimeCalculation
{
  public:
    enum UsedMethod
    {
      Maximum, //!< use the maximum value among the active formulas
      Minimum, //!< use the minimum value among the active formulas
      Average, //!< use the maximum the average value od active formulas
      UserChoosenFormula //!< use a specific formula (see userChoosenMethod())
    };

    //! Returns the calculted value with \a parameters
    ReosDuration concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters );

    //! Sets the list of formulas that will be used to calculate the concentration time
    void setActiveFormula( const QStringList &formulaNames );

    //! Returns the list of formulas that will be used to calculate the concentration time
    QStringList activeFormulas() const;

    //! Returns the method used for the calculation
    UsedMethod usedMethod() const;

    //! Returns the method used for the calculation
    void setUsedMethod( const UsedMethod &returnMethod );

    //! Returns the user choosen formula
    QString userChoosenFormula() const;

    //! Sets the user choosen formula
    void setUserChoosenFormula( const QString &userDefinedFormula );

    //! Returns whether the calculation has ever benn done since the construction of this instance
    bool alreadyCalculated() const;

    //! Sets whether the calculation has ever benn done since the construction of this instance
    void setAlreadyCalculated( bool alreadyCalculated );

    ReosEncodedElement encode() const;
    static ReosConcentrationTimeCalculation decode( const ReosEncodedElement &element );

  private:
    QStringList mActiveFormulas;
    UsedMethod mUsedMethod = Average;
    QString mUserChoosenFormula;
    bool mAlreadyCalculated = false;
};

//! Model that can be used to display formulas in a table view
class REOSCORE_EXPORT ReosConcentrationTimeFormulasModel : public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosConcentrationTimeFormulasModel( ReosConcentrationTimeFormulasRegistery *registery, QObject *parent );

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    QVariant data( const QModelIndex &index, int role ) const override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    //! Sets the \a parameters
    void setParameters( ReosConcentrationTimeFormula::Parameters parameters );

    //! Returns the parameters used
    ReosConcentrationTimeFormula::Parameters parameters() const;

    //! Sets the active \a formulas
    void setActiveFormulas( const QStringList &formulas );

    //! Returns the active formulas
    QStringList activeFormulas() const;

    //! Sets the choosen formula from this \index
    void setChoosenFormula( const QModelIndex &index );

    //! Sets the choosen \a formula
    void setChoosenFormula( const QString &formula );

    //! Returns the choosen formula
    QString choosenFormula() const;

    //! Sets if the choosen formula has to be highlighted
    void highlightChoosenFormula( bool b );

    //! Returns active formula name and corresponding resutls under a text tab format
    QString textData() const;

  public slots:
    //! Sets the current time unit used to display results
    void setCurrentTimeUnit( const ReosDuration::Unit &currentTimeUnit );

  signals:
    //! emitted when the active formulas changed
    void activeFormulasChanged();

  private:
    ReosConcentrationTimeFormulasRegistery *mRegistery = nullptr;
    ReosConcentrationTimeFormula::Parameters mParameters;
    QStringList mActiveFormulas;
    QString mChoosenFormula;
    bool mHighLightChoosenFormula = true;
    ReosDuration::Unit mCurrentTimeUnit = ReosDuration::minute;
};

#endif // REOSCONCENTRATIONTIMECALCULATION_H
