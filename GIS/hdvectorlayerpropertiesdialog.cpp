/***************************************************************************
                      hdvectorlayerpropertiesdialog.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "hdvectorlayerpropertiesdialog.h"
#include "ui_hdvectorlayerpropertiesdialog.h"

HdVectorLayerPropertiesDialog::HdVectorLayerPropertiesDialog(QgsVectorLayer *layer, QgsMapCanvas *canvas) :
    QDialog(canvas),
    ui(new Ui::HdVectorLayerPropertiesDialog),layer(layer),
    canvas(canvas)
{
    ui->setupUi(this);

    //install source tab
    ui->lineEditLayerName->setText(layer->name());
    ui->lineEditSource->setText(layer->source());
    crsSelection=new QgsProjectionSelectionWidget(this);
    crsSelection->setCrs(layer->crs());
    crsSelection->setOptionVisible(QgsProjectionSelectionWidget::LayerCrs,true);
    ui->groupBoxCRS->setLayout(new QVBoxLayout);
    ui->groupBoxCRS->layout()->addWidget(crsSelection);

    QString myStyle = QgsApplication::reportStyleSheet();
    myStyle.append( QStringLiteral( "body { margin: 10px; }\n " ) );
    ui->textBrowser->clear();
    ui->textBrowser->document()->setDefaultStyleSheet( myStyle );
    ui->textBrowser->setOpenLinks( false );
    ui->textBrowser->setText(layer->htmlMetadata());
    connect(ui->textBrowser,&QTextBrowser::anchorClicked,this,&HdVectorLayerPropertiesDialog::urlClicked);

    //install the symbologie
    ui->symbologyContent->setLayout(new QHBoxLayout);
    renderDialog=new QgsRendererPropertiesDialog(layer,QgsStyle::defaultStyle(),true,this);
    renderDialog->setDockMode(false);
    ui->symbologyContent->layout()->addWidget(renderDialog);
    renderDialog->setMapCanvas(canvas);

    QPushButton  *applyButton=ui->buttonBox->button(QDialogButtonBox::Apply);
    connect(applyButton,&QPushButton::clicked,this,&HdVectorLayerPropertiesDialog::apply);
    connect(ui->buttonBox->button(QDialogButtonBox::Ok),&QPushButton::clicked,this,&HdVectorLayerPropertiesDialog::updateSettings);

    ReosSettings settings;
    if (!settings.contains(QStringLiteral("Windows/VectorLayerProperties/tab")))
        settings.setValue(QStringLiteral("Windows/VectorLayerProperties/tab"),ui->tabWidget->indexOf(ui->tabSymbology));

    int indexTab=settings.value(QStringLiteral("Windows/VectorLayerProperties/tab")).toInt();
    if ((indexTab<ui->tabWidget->count())&&(indexTab>=0))
        ui->tabWidget->setCurrentIndex(indexTab);

}

HdVectorLayerPropertiesDialog::~HdVectorLayerPropertiesDialog()
{
    delete ui;
}

void HdVectorLayerPropertiesDialog::apply()
{
    renderDialog->apply();
    layer->setCrs(crsSelection->crs(),true);
    canvas->refresh();

}

void HdVectorLayerPropertiesDialog::urlClicked(const QUrl &url)
{
    QFileInfo file( url.toLocalFile() );
    if ( file.exists() && !file.isDir() )
        QgsGui::instance()->nativePlatformInterface()->openFileExplorerAndSelectFile( url.toLocalFile() );
    else
        QDesktopServices::openUrl( url );
}

void HdVectorLayerPropertiesDialog::updateSettings()
{
    ReosSettings settings;
    settings.setValue(QStringLiteral("Windows/VectorLayerProperties/tab"),ui->tabWidget->currentIndex());
}

void HdVectorLayerPropertiesDialog::closeEvent(QCloseEvent *event)
{
    updateSettings();
    QDialog::closeEvent(event);
}
