package cz.rozehra.signalProcessing.visualization;

import cz.rozehra.signalProcessing.Spectrogram;

import javax.swing.*;
import java.awt.*;

class FrequencyScaleDrawer extends JPanel {
    public FrequencyScaleDrawer() {
        setPreferredSize(new Dimension(60, 512));
    }


    private Spectrogram spectrogram;

    public void addSpectrogram(Spectrogram spectrogram) {
        this.spectrogram = spectrogram;
    }


    @Override
    public void paintComponent(Graphics g) {
        if (spectrogram != null) {
            int hundredsOfHertz = (int)(spectrogram.maxFrequency() / 100.0);

            for (int i = 0; i <= hundredsOfHertz; ++i) {
                int verticalPosition = 512 - (int)(i * 100.0 / spectrogram.bandWidth());
                if (i % 10 == 0)  {
                    g.drawLine(54, verticalPosition, 60, verticalPosition);

                    int digitsCount = (int)(Math.log10(100 * i));
                    String leadingSpaces = "";
                    //for (int j = digitsCount; j < 5; ++j) { leadingSpaces += " "; }

                    g.drawString(leadingSpaces + (100 * i) + " Hz", 0, verticalPosition);
                }
                else { g.drawLine(58, verticalPosition, 60, verticalPosition); }
            }
        }
    }
}
