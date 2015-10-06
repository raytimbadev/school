/**
* The POSTGRESQL interface implentation file
* Note: Secrets file not included in commit for
* security reasons one must thus be provided
*/

package sql; 
import java.io.IOException;
import java.sql.SQLException; 
import java.io.FileNotFoundException; 
import java.sql.Connection;
import java.beans.PropertyVetoException; 
import java.sql.DriverManager;
import java.sql.PreparedStatement; 
import java.sql.Statement; 
import java.sql.ResultSet; 
import java.sql.SQLException;
import java.sql.Statement;
import java.io.BufferedReader; 
import java.io.FileReader; 
import java.io.File; 
import org.apache.commons.dbcp2.BasicDataSource;


class DataSource {

    private static DataSource datasource;
    private BasicDataSource ds;

    public DataSource(String username, String password, String url) throws IOException, SQLException, PropertyVetoException {
        ds = new BasicDataSource();
        ds.setDriverClassName("org.postgresql.Driver");
        ds.setUsername(username);
        ds.setPassword(password);
        ds.setUrl(url);
    }

    public Connection getConnection() throws SQLException {
        return this.ds.getConnection(); 
    }
}

public class sqlInterfaceImpl implements sql.sqlInterface {
	private static String url; 
	private	static String user; 
	private static String password;
	private static boolean initialized = false;
	private static DataSource datasource; 	
	
	/**
	* Based on the initilized state boolean either returns or initializes
	* the required data by parsing a secrets file, the secret file
	* in src is automatically coppied to build by ant on compile
	*/
	private void initialize() {
		if(initialized) {
			return;
		}
		try{
			File f = new File("sql.secrets");
			if(f.exists() && !f.isDirectory()) { 
				BufferedReader br = new BufferedReader(new FileReader("sql.secrets"));
				url=br.readLine();
				user=br.readLine();
				password=br.readLine();
			} else {
				System.out.println("secrets file not found in project root directory"); 
				throw new FileNotFoundException();
			}
			datasource = new DataSource(user,password,url); 
		} catch	(Exception e) {
			e.printStackTrace();
		}
		initialized = true;
	}

	@Override	
	public void writeTransaction(String[] t) {
		Connection con = null;
		Statement ps=null;
		try{
			initialize();
			con=datasource.getConnection();
			ps = con.createStatement();
			con.setAutoCommit(false);
			for(String s : t) {
				ps.executeUpdate(s);
			}
			con.commit(); 
		}catch (SQLException e) {
			e.printStackTrace();
			if(con != null) {
				try {
					con.rollback();
				} catch(Exception ex) {
					ex.printStackTrace(); 
				}
			}
		}finally {
			try {
				if(ps != null)
					ps.close();
				if(con != null)
					con.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	@Override
	public int[] returningTransaction(String[] t) {
	    Connection con = null;
        Statement ps=null;
		int[] arr = new int[t.length]; 
        try{
            initialize();
            con=datasource.getConnection();
            ps = con.createStatement();
            for(int i = 0; i < t.length; i++) {
                ps.executeUpdate(t[i],Statement.RETURN_GENERATED_KEYS);
				ResultSet rs = ps.getGeneratedKeys();
				if( rs.next() ) {
					arr[i] = rs.getInt(1); 
				}
            }
            con.commit(); 
        }catch (SQLException e) {
            e.printStackTrace();
        } finally {
            try {
                if(ps != null)
                    ps.close();
                if(con != null)
                    con.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
		return arr; 
	}
	@Override
	public ResultSet Retrieve(String query) {
		Connection con=null; 
		PreparedStatement ps=null;
		ResultSet rs=null;  
		try{
			initialize(); 
			con = datasource.getConnection();  
			ps = con.prepareStatement(query);
			rs = ps.executeQuery(); 
			
		} catch (Exception e) {
			e.printStackTrace(); 
		} finally {
			try {
				if(ps != null)
					ps.close();
				if(con != null)
					rs.close(); 
			} catch (Exception e) {
				e.printStackTrace(); 
			}
		}	

		return rs;
	}
} 
