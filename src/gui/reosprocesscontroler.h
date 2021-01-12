#ifndef REOSPROCESSCONTROLER_H
#define REOSPROCESSCONTROLER_H

#include <QDialog>
#include <QTimer>

#include "reosprocess.h"

namespace Ui
{
  class ReosProcessControler;
}

/**
 * Gui class derived from QDialog that display information, progess bar and cancelation button for a ReosProcess
 */
class ReosProcessControler : public QDialog
{
    Q_OBJECT

  public:
    //! Constructor with a \a process
    explicit ReosProcessControler( ReosProcess *process, QWidget *parent = nullptr );

    ~ReosProcessControler();

  private slots:
    void refresh();
    void onCancel();
    void onFinished();

  private:
    Ui::ReosProcessControler *ui;
    ReosProcess *mProcess = nullptr;

    QTimer mTimer;
    int mRefreshInterval = 20;
};

#endif // REOSPROCESSCONTROLER_H
