/***************************************************************************
                      reosform.h
                     --------------------------------------
Date                 : 31-03-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                :   projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSFORM_H
#define REOSFORM_H

#include <QWidget>
#include <QHBoxLayout>
#include <QLineEdit>
#include <QLabel>
#include <QComboBox>
#include <QCheckBox>
#include <QAction>
#include <QToolButton>

#include "../quantity/reosarea.h"
#include "../quantity/reosduration.h"

class ReosForm;
class ReosFormParameterSimpleBool;

//*********************************************************************************************
//****************************  Base Parameter class     **************************************
//*********************************************************************************************

class ReosFormParameterEditor: public QWidget
{
    Q_OBJECT
  public:
    ReosFormParameterEditor();
    virtual ~ReosFormParameterEditor() {}

  signals:
    void askForCalculate();

  protected:
    QToolButton *toolButtonAskForCalculate;

    QString setToSystemLocale( QString str );
    QString setToCLocale( QString str );

  private slots:
    void askForCalculateClicked();

};


class ReosFormObject: public QObject
{
  public:
    ReosFormObject( QObject *parent );

    virtual QWidget *getWidget( bool stretch = true ) = 0;
};

class ReosFormParameter: public ReosFormObject
{
    Q_OBJECT
  public:
    ReosFormParameter( ReosForm *parent );
    virtual ~ReosFormParameter() {}
    virtual QWidget *getWidget( bool stretch = true );
    virtual void hideWidget() {emit askForHideWidget();}

  signals:
    void askForValue();
    void valueEdited();
    void askForHideWidget();

  protected slots:
    void valueAsked();
    virtual ReosFormParameterEditor *makeEdit() = 0;



};

//*********************************************************************************************
//********************************Simple parameter*********************************************
//*********************************************************************************************

class ReosFormParameterEditorSimple: public ReosFormParameterEditor
{
    Q_OBJECT
  public:
    ReosFormParameterEditorSimple( QString nameField, QString value );


    ReosFormParameterEditorSimple( QString nameField, QString value, QString unit );

    virtual ~ReosFormParameterEditorSimple();


    void setAlignment( Qt::Alignment a );

    void enableCalculateButton();

  public slots:
    void updateValue( QString val );
    void enableEditor( bool b );
    void disableEditor( bool b );
    void setEditable( bool b );

  signals:
    void changed( QString value );

  protected:
    void setValueEditText( QString str );

  private slots:
    void valueHasBeenEdited();

  private:
    QHBoxLayout *lay;
    QLineEdit *valueEdit;

};

class ReosFormParameterSimple: public ReosFormParameter
{
    Q_OBJECT
  public:
    ReosFormParameterSimple( QString name, ReosForm *parent = nullptr );
    virtual ~ReosFormParameterSimple() override {}

    QString getName() const;
    void setName( const QString &value );

    virtual bool isDefined() const = 0;
    virtual void setUndefined() = 0;

    void enableCalculateButton();

    void linkEditableWithBool( ReosFormParameterSimpleBool *boolParameter, bool editableIfTrue );
    void linkEnableWithBool( ReosFormParameterSimpleBool *boolParameter, bool enableIfTrue );

  signals:
    void valueChanged( QString value );
    void enable( bool b );
    void editableEditor( bool b );


  public slots:
    void enableEditor( bool b );
    void disableEditor( bool b );
    void setEditable( bool b );
    void setUneditable( bool b );

  protected:
    bool calculateButton = false;
    bool disable = false;
    bool editable = true;

  private:
    QString name;


    virtual ReosFormParameterEditorSimple *instantiateEditor() = 0;

    virtual QString getStringValue() = 0;

  protected slots:
    virtual ReosFormParameterEditor *makeEdit() override;

    virtual void valueChangeByEditor( QString v ) = 0;

};

class ReosFormParameterEditorSimpleWithVariableUnit: public ReosFormParameterEditorSimple
{
    Q_OBJECT
  public:
    ReosFormParameterEditorSimpleWithVariableUnit( QString nameField, QString value, QStringList units, int unitPos );

    virtual ~ReosFormParameterEditorSimpleWithVariableUnit() override;

  signals:
    void changeUnit( int i );


  private slots:

    void unitChanged( int i );

};


//********************************double simple parameter*********************************************


class ReosFormParameterSimpleDouble: public ReosFormParameterSimple
{
    Q_OBJECT
  public:
    ReosFormParameterSimpleDouble( QString name, double value, ReosForm *parent, QString unit = "" );

    double getValue() const;
    void setValue( double value );

    QString getUnit() const;
    void setUnit( const QString &value );

    void setPrecision( int p );

    virtual bool isDefined() const override;

    virtual void setUndefined() override;

    double getNoData() const;
    void setNoData( double value );

    void setAlignment( Qt::Alignment a )
    {
      alignment = a;
    }

  private:
    double value;
    double noData = -99999;
    QString unit;
    Qt::Alignment alignment = Qt::AlignRight;

    // HdFormParameterSimple interface
  private:
    ReosFormParameterEditorSimple *instantiateEditor() override;

    int precision = 6;

    QString getStringValue() override;

  protected slots:
    void valueChangeByEditor( QString v ) override;

};

//********************************String parameter*********************************************

class ReosFormParameterSimpleString: public ReosFormParameterSimple
{
    Q_OBJECT
  public:
    ReosFormParameterSimpleString( QString name, QString value, ReosForm *parent );
    virtual ~ReosFormParameterSimpleString() override {}


    QString getValue() const;
    void setValue( const QString &v );

    virtual bool isDefined() const override;

    virtual void setUndefined() override;

  private:
    QString value;

    // HdFormParameterSimple interface
  private:
    ReosFormParameterEditorSimple *instantiateEditor() override;

    QString getStringValue() override;

  protected slots:
    void valueChangeByEditor( QString v ) override;
};

//********************************Variable unit parameters*********************************************

class ReosFormParameterSimpleUnitVariable: public ReosFormParameterSimple
{
    Q_OBJECT
  public:
    ReosFormParameterSimpleUnitVariable( QString name, ReosForm *parent );

    QString getStringUnit() const;


  signals:
    void unitChange( int i );

  protected slots:
    virtual ReosFormParameterEditor *makeEdit() override;

    void valueChangeByEditor( QString v ) override;

    void unitChangeByEditor( int i );

    QString getStringValue() override;

    virtual QStringList getUnits() const = 0;


  private:

    ReosFormParameterEditorSimpleWithVariableUnit *instantiateEditor() override;

    virtual double getValueDouble() const = 0;
    virtual void setValueFromDouble( double d ) = 0;
    virtual int getCurrentUnit() const = 0;
    virtual double getValueInUnit( int i ) const = 0;
    virtual void setUnit( int i ) = 0;
};

class ReosFormParameterSimpleArea: public ReosFormParameterSimpleUnitVariable
{
    Q_OBJECT
  public:
    ReosFormParameterSimpleArea( QString name, ReosArea value, ReosForm *parent ): ReosFormParameterSimpleUnitVariable( name, parent ),
      value( value )
    {}

    ReosArea getValue() const;
    void setValue( const ReosArea &value );

    virtual bool isDefined() const override;

    virtual void setUndefined() override;

  private:
    ReosArea value;


    // HdFormParameterSimpleUnitVariable interface
  private:
    QStringList getUnits() const override;

    double getValueDouble() const override;

    virtual void setValueFromDouble( double d ) override;

    int getCurrentUnit() const override;

    virtual double getValueInUnit( int i ) const override;

    virtual void setUnit( int i ) override;

};

class ReosFormParameterSimpleDuration: public ReosFormParameterSimpleUnitVariable
{
    Q_OBJECT
  public:
    ReosFormParameterSimpleDuration( QString name, ReosDuration value, ReosForm *parent );

    ReosDuration getValue() const;
    void setValue( const ReosDuration &v );


    virtual bool isDefined() const override;

    virtual void setUndefined() override;

    QStringList getUnits() const override;

  private:
    ReosDuration value;


    // HdFormParameterSimpleUnitVariable interface


    double getValueDouble() const override;

    virtual void setValueFromDouble( double d ) override;

    int getCurrentUnit() const override;

    virtual double getValueInUnit( int i ) const override;

    virtual void setUnit( int i ) override;


};
//*********************************************************************************************
//********************************Simple parameter*********************************************
//*********************************************************************************************

class ReosFormParameterEditorCheckBox: public ReosFormParameterEditor
{
    Q_OBJECT
  public:
    ReosFormParameterEditorCheckBox( QString nameField, bool value );

  public slots:
    void updateValue( bool val );


  signals:
    void changed( bool value );

  private:
    QCheckBox *checkBox;


  private slots:
    void valueHasBeenEdited();

};

class ReosFormParameterSimpleBool: public ReosFormParameter
{
    Q_OBJECT
  public:
    ReosFormParameterSimpleBool( QString name, bool value, ReosForm *parent );

    bool getValue() const;
    void setValue( bool value );

  signals:
    void valueChanged( bool b );

  private:
    QString name;
    bool value;


  protected slots:
    void valueChangeByEditor( bool v );

    // HdFormParameter interface
  protected slots:
    ReosFormParameterEditor *makeEdit() override;
};

//*********************************************************************************************
//********************************     Action     *********************************************
//*********************************************************************************************
class ReosFormAction: public ReosFormObject
{
  public:
    ReosFormAction( ReosForm *parent, const QIcon &icon, const QString &text );

    void setChecked( bool b )
    {
      mAction->setChecked( b );
    }

    void setCheckable( bool b )
    {
      mAction->setCheckable( b );
    }

    QAction *action() const;

  private:
    QAction *mAction;

    // ReosFormObject interface
  public:
    QWidget *getWidget( bool stretch = true ) override
    {
      Q_UNUSED( stretch );
      QToolButton *toolButton = new QToolButton;
      toolButton->setDefaultAction( mAction );

      return toolButton;

    }
};

//*********************************************************************************************
//********************************     Text     *********************************************
//*********************************************************************************************
class ReosFormText: public ReosFormObject
{
    Q_OBJECT
  public:
    ReosFormText( ReosForm *parent, const QString &text );
    void setText( const QString &text );

  signals:
    void textChanged( const QString &text );

  private:
    QString mText;

    // ReosFormObject interface
  public:
    QWidget *getWidget( bool stretch = true ) override
    {
      Q_UNUSED( stretch );
      QLabel *label = new QLabel( mText );
      connect( this, &ReosFormText::textChanged, label, &QLabel::setText );

      return label;

    }
};

//*********************************************************************************************
//******************************  Base Form class     *****************************************
//*********************************************************************************************

class ReosForm: public ReosFormObject
{
    Q_OBJECT
  public:
    ReosForm( QObject *parent = nullptr ): ReosFormObject( parent ) {}
    virtual ~ReosForm() override {}
    const QList<ReosFormObject *> &getFormObjectList() const;

    void addForm( ReosForm *childForm );
    void addParamater( ReosFormParameter *param );
    void addForm( ReosFormObject *formObject );
    void addSeparator();
    bool isSeparatorPresent( int i ) const;

    void setOrientation( Qt::Orientation orientation );
    Qt::Orientation orientation() const;

  public slots:
    virtual void parameterEdit();

  signals:
    void parameterEdited();

  private:
    QList<ReosFormObject *> attachedFormObjects;
    QList<int> separatorsPosition;
    Qt::Orientation mOrientation = Qt::Vertical;

    // HdFormObject interface
  public:
    QWidget *getWidget( bool stretch = true ) override;
};

class ReosFormWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosFormWidget( ReosForm *form = nullptr, QWidget *parent = nullptr, Qt::Orientation orientation = Qt::Vertical, bool stretch = true );

  signals:

  public slots:

  private:
    QLayout *addSeparator( Qt::Orientation orientation = Qt::Horizontal );
};



//*********************************************************************************************



#endif // REOSFORM_H
