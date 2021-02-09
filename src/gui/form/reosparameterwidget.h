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
class QDateTimeEdit;

#include "reosparameter.h"

class ReosParameterWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterWidget( QWidget *parent = nullptr );

    //! Sets the keyboard focus on the edit part of the widget
    virtual void setFocusOnEdit() = 0;
    static ReosParameterWidget *createWidget( ReosParameter *parameter, QWidget *parent = nullptr );

  public slots:
    //! Updates the dispayed value from the parameter
    virtual void updateValue() = 0;
    //! Sets the parameter value with the displayed value
    virtual void applyValue() = 0;
    virtual void askDerivation();

  protected:
    void finalizeWidget();
    void setParameter( ReosParameter *param );

    ReosParameter *mParameter = nullptr;

  private:
    QLabel *mLabelName = nullptr;

    QToolButton *mDerivationButton = nullptr;

};

class ReosParameterInLineWidget : public ReosParameterWidget
{
  public:
    ReosParameterInLineWidget( QWidget *parent = nullptr );
    void setFocusOnEdit() override;

  protected:
    void setTextValue( double value );
    void setTextValue( const QString &str );
    double value() const;
    QString textValue() const;
  private:
    QLineEdit *mLineEdit = nullptr;
};

class ReosParameterStringWidget : public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterStringWidget( QWidget *parent = nullptr );
    explicit ReosParameterStringWidget( ReosParameterString *string, QWidget *parent = nullptr );

    void setString( ReosParameterString *string );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "string" );}

  private:
    ReosParameterString *stringParameter();
};

class ReosParameterAreaWidget: public ReosParameterInLineWidget
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

class ReosParameterSlopeWidget: public ReosParameterInLineWidget
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


class ReosParameterDurationWidget: public ReosParameterInLineWidget
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

class ReosParameterDateTimeWidget: public ReosParameterWidget
{
  public:
    explicit ReosParameterDateTimeWidget( QWidget *parent = nullptr );
    explicit ReosParameterDateTimeWidget( ReosParameterDateTime *dateTime, QWidget *parent = nullptr );

    void setDateTime( ReosParameterDateTime *dateTime );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "date-time" );}

  private:
    QDateTimeEdit *mDateTimeEdit = nullptr;
    ReosParameterDateTime *dateTimeParameter() const;


};
#endif // REOSPARAMETERWIDGET_H
