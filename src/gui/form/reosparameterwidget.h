/***************************************************************************
  reosparameterwidget.h - ReosParameterWidget

 ---------------------
 begin                : 22.1.2021
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
#ifndef REOSPARAMETERWIDGET_H
#define REOSPARAMETERWIDGET_H

#include <QWidget>

class QLabel;
class QLineEdit;
class QComboBox;
class QToolButton;

#include "reosparameter.h"

class ReosParameterWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterWidget( QWidget *parent = nullptr );

    void setFocusOnEdit();

    static ReosParameterWidget *createWidget( ReosParameter *parameter, QWidget *parent = nullptr );

  public slots:
    virtual void updateValue() = 0;
    virtual void applyValue() = 0;
    virtual void askDerivation();

  protected:
    void finalizeWidget();
    void setTextValue( double value );
    void setTextValue( const QString &str );
    double value() const;
    QString textValue() const;
    void setParameter( ReosParameter *param );

    ReosParameter *mParameter = nullptr;

  private:
    QLineEdit *mLineEdit = nullptr;
    QLabel *mLabelName = nullptr;
    QToolButton *mDerivationButton = nullptr;

};

class ReosParameterStringWidget : public ReosParameterWidget
{
  public:
    explicit ReosParameterStringWidget( QWidget *parent = nullptr );
    explicit ReosParameterStringWidget( ReosParameterString *string, QWidget *parent = nullptr );

    void setString( ReosParameterString *string )
    {
      setParameter( string );
    }
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "string" );}

  private:
    ReosParameterString *stringParameter();
};

class ReosParameterAreaWidget: public ReosParameterWidget
{
  public:
    explicit ReosParameterAreaWidget( QWidget *parent = nullptr );
    explicit ReosParameterAreaWidget( ReosParameterArea *area, QWidget *parent = nullptr );

    void setArea( ReosParameterArea *area );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "area" );}

  private:
    ReosParameterArea *areaParameter() const;
    QComboBox *mUnitCombobox = nullptr;
};

class ReosParameterSlopeWidget: public ReosParameterWidget
{
  public:
    explicit ReosParameterSlopeWidget( QWidget *parent = nullptr );
    explicit ReosParameterSlopeWidget( ReosParameterSlope *slope, QWidget *parent = nullptr );

    void setSlope( ReosParameterSlope *slope );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "slope" );}

  private:
    ReosParameterSlope *slopeParameter() const;
    QLabel *mLabelSlopeUnit = nullptr;
    int mFactor = 100;
};


class ReosParameterDurationWidget: public ReosParameterWidget
{
  public:
    explicit ReosParameterDurationWidget( QWidget *parent = nullptr );
    explicit ReosParameterDurationWidget( ReosParameterDuration *area, QWidget *parent = nullptr );

    void setDuration( ReosParameterDuration *duration );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "duration" );}

  private:
    ReosParameterDuration *durationParameter() const;
    QComboBox *mUnitCombobox = nullptr;
};
#endif // REOSPARAMETERWIDGET_H
