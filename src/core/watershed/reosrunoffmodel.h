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
#include <QAbstractItemModel>

#include "reosdataobject.h"
#include "reosduration.h"
#include "reosmodule.h"

class ReosTimeSerieConstantInterval;
class ReosParameter;
class ReosParameterDouble;
class ReosParameterString;
/**
 * Abstract class that represent a runoff calculation on a rainfall,
 * instance of a derived class should contain the parameter necessary to apply the model on a rainfall
 */
class ReosRunoffModel : public QObject
{
    Q_OBJECT
  public:

    //! Returns the runoff type
    virtual QString runoffType() const = 0;

    //! Returns the list of parameters
    virtual QList<ReosParameter *> parameters() const = 0;

    //! Returns the name of this runoff model
    ReosParameterString *name() const;

    //! Applies the model on the \a rainfall and put the result in runoffResult
    virtual bool applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, QVector<double> &runoffResult ) = 0;

    virtual ReosEncodedElement encode() const = 0;

    //! Returns the unique Id of this runoff model
    QString uniqueId() const;

  signals:
    void modelChanged();

  protected:
    ReosRunoffModel( const QString &name, QObject *parent = nullptr );
    ReosRunoffModel( const ReosEncodedElement &element, QObject *parent = nullptr );
    void connectParameters();
    void encodeBase( ReosEncodedElement &element ) const;

  private:
    ReosParameterString *mName;
    QString mUniqueId;
};

//! Class that contains several models with same type
class ReosRunoffModelCollection
{
  public:
    ReosRunoffModelCollection() = default;
    /**
     * Constructr of the collection
     * \param type the type od the runoff models
     * \param displayedText the text taht will be displayed to represent this collection
     * \param icon the icon that cill be displyed to represent this collection
     */
    ReosRunoffModelCollection( const QString &type, const QString &displayedText, const QPixmap &icon = QPixmap() );

    //! Returns the type odf this collection
    QString type() const;

    //! Returns the text that is used to represents this collection
    QString displayedText() const;

    //! Returns the icon used to represents this collection
    QPixmap icon() const {return mIcon;}

    //! Returns the count of models in this collection
    int runoffModelsCount() const;

    //! Adds a model to ths collection
    void addModel( ReosRunoffModel *runoffModel );

    //! Remove the modl at potions \a i
    void removeRunoffModel( int i );

    //! Returns the position of \a runoffModel in this collection, returns -1 if no present
    int index( ReosRunoffModel *runoffModel ) const;

    //! Returns the runoff model at position \a i in this collection
    ReosRunoffModel *runoffModel( int i ) const;

    //! Returns whether this collection the model named \a name
    bool containsModelRunoff( const QString &name ) const;

    //! Deletes all runoff models
    void clearCollection();

    //! Returns a runoff model consiering its unique id
    ReosRunoffModel *runoffModelByUniqueId( const QString &uniqueId ) const;

  private:
    QString mType;
    QString mDisplayedText;
    QPixmap mIcon;
    QList<ReosRunoffModel *> mRunoffModels;
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

//! Class that represents a model containing runoff model collections and theirs runoff models
class ReosRunoffModelModel : public QAbstractItemModel
{
  public :
    ReosRunoffModelModel( QObject *parent = nullptr ): QAbstractItemModel( parent )
    {}

    QModelIndex index( int row, int column, const QModelIndex &parent ) const override;
    QModelIndex parent( const QModelIndex &child ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    //! Adds a model to the collection, does not take ownership, return false if the model is not effectivly added
    bool addModel( ReosRunoffModel *runoffModel );

    //! Adds a new empty collection
    void addCollection( const QString &type, const QString &displayedName, const QPixmap &icon = QPixmap() );

    //! Returns the runoff model corresponding to the \a index
    ReosRunoffModel *runoffModel( const QModelIndex &index ) const;

    //! Returns whether the model contains a runoff model named \a name
    bool containsRunoffModel( const QString &name ) const;

    //! removes and delete the runoff model \a runoffModel
    void removeRunoffModel( ReosRunoffModel *runoffModel );

    //! Returns the type of the collection or runoff models corresponding to \a index
    QString indexToType( const QModelIndex &index ) const;

    //! Returns the index  corresponding to the runoff model \a runoffModel
    QModelIndex runoffModelToIndex( ReosRunoffModel *runoffModel ) const;

    //! Returns whethr the model containd aleast one runoff model
    bool hasData() const;

    //! Removes and delete all runoff models
    void clear();

    //! Returns all the models encoded
    QList<ReosEncodedElement> encodeModels() const;

    //! Returns a runoff model consiering its unique id
    ReosRunoffModel *runoffModelByUniqueId( const QString &uniqueId ) const;

  private:
    QMap<QString, ReosRunoffModelCollection> mRunoffCollections;

    QModelIndex typeToIndex( const QString &runoffType ) const;
    ReosRunoffModel *indexToRunoffModel( const QModelIndex &index ) const;
};

/**
 * Sinlgeton class that register and handle all the runoff models created
 */
class ReosRunoffModelRegistery : public ReosModule
{
  public:
    //! Intantiate the singleton
    static void instantiate( ReosModule *parent = nullptr );

    //! Returns whether the singleton is instantiate
    static bool isInstantiate();

    //! Returns the instance of the singleton
    static ReosRunoffModelRegistery *instance();

    //! Adds a new empty runoff model singleton
    void addModelCollection( const QString &type, const QString displayedText, const QPixmap &icon = QPixmap() );

    //! Adds a description (optional) for the collection of type \a type
    void addDescription( const QString &type, const QString desription );

    //! Returns the description for the collection of type \a type
    QString modelDescription( const QString &type ) const;

    //! Creates a new runoff model of type \a type names \a name
    ReosRunoffModel *createModel( const QString &type, const QString &name );

    //! Creates a runnof from the encoded \a element
    ReosRunoffModel *createModel( const ReosEncodedElement &element );

    //! Returns the data model used to handle the runoff models
    ReosRunoffModelModel *model() const;

    //! Convenient method used to create automatically a name for a runff model depending of its type
    QString createRunoffModelName( const QString &type );

    //! Saves the data to file
    bool saveToFile( const QString &fileName, const QString &header ) const;

    //! Clears all current data and load new from files
    bool loadFromFile( const QString &fileName, const QString &header );

    //! Returns a runoff model consiering its unique id
    ReosRunoffModel *runoffModelByUniqueId( const QString &uniqueId ) const;

  private:
    ReosRunoffModelRegistery( QObject *parent );

    ReosEncodedElement encode() const;
    bool decode( const ReosEncodedElement &element );

    static ReosRunoffModelRegistery *sRegisteryInstance;
    ReosRunoffModelModel *mModel = nullptr;
    QMap<QString, QString> mModelDescriptions;
};

//*****************************************************************************

class ReosRunoffConstantCoefficientModel: public ReosRunoffModel
{
  public:
    ReosRunoffConstantCoefficientModel( const QString &name, QObject *parent = nullptr );

    QString runoffType() const override {return QStringLiteral( "constant-coefficient" );}
    QList<ReosParameter *> parameters() const override;
    bool applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, QVector<double> &runoffResult ) override;
    ReosEncodedElement encode() const override;
    static ReosRunoffConstantCoefficientModel *create( const ReosEncodedElement &element, QObject *parent = nullptr );

    //! Returns the constant coefficient parameter
    ReosParameterDouble *coefficient();

  private:
    ReosRunoffConstantCoefficientModel( const ReosEncodedElement &element, QObject *parent = nullptr );
    ReosParameterDouble *mCoefficient;
};

//*****************************************************************************


#endif // REOSRUNOFFMODEL_H
