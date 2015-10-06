/**
* Interface file for the POSTGRESQL operations
*/

package sql; 

import java.sql.Connection; 
import java.sql.DriverManager; 
import java.sql.SQLException;
import java.sql.Statement; 
import java.sql.ResultSet; 


public interface sqlInterface {
	public void writeTransaction(String[] t);
	public int[] 	returningTransaction(String[] t);
	public ResultSet Retrieve(String query);
} 
