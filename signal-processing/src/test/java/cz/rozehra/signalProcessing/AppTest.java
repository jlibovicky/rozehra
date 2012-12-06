package cz.rozehra.signalProcessing;

import java.io.InputStream;

import cz.rozehra.signalProcessing.WaveFileReader;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for simple App.
 */
public class AppTest extends TestCase {
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public AppTest( String testName ) {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite() {
        return new TestSuite( AppTest.class );
    }
    
    private WaveFileReader wave = null;
    
    public void testWaveFile() {
        InputStream resource = AppTest.class.getClassLoader().getResourceAsStream("secondOf440.wav");
        wave = new WaveFileReader(resource);        
        System.out.println(wave.toString());
        wave.plot("secondOf440.png");
    }
}
