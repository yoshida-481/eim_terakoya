package common.tools.internal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMException;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.RecursiveTableEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * ドキュメント管理用オブジェクトタイプユーティリティ
 *
 */
public class AppObjectTypeUtils {

	/**
	 * タイプ定義名称からそのタイプを継承したクラスも含めたクラスから生成されたオブジェクトのIDを取得する
	 * @param typeDefineName タイプ定義名称
	 * @return 取得されたオブジェクトIDのリスト
	 * @throws EIMException
	 */
	public static List<Integer> getListByInheritanceTypeName(String typeDefineName) throws EIMException{
		List<Integer> result = new ArrayList<Integer>();
		try{
			EIMSession sess = EIMThreadContext.getEIMSession();
			String sql =
					"select ID from EIMOBJ where type in (" +
						DatabasePlugInLoader.getPlugIn().
								getQueryStringWithRecursive(RecursiveTableEnum.EIMOBJTYPE_CHILDREN, new String[] {"ID"}, "name = ?", false) +
						DatabasePlugInLoader.getPlugIn().
								getQueryStringSelectRecursive(RecursiveTableEnum.EIMOBJTYPE_CHILDREN, new String[] {"ID"}, "name = ?", false) +
					") order by id";
			PreparedStatement ps = sess.getDBConnection().prepareStatement(sql);
			ps.setString(1, typeDefineName);
			ResultSet rset = ps.executeQuery();
			while (rset.next()) {
				result.add(Integer.parseInt(rset.getString("ID")));
			}
			rset.close();
			ps.close();
		}
		catch(Exception e){
			EIMSysException eime = new EIMSysException();
			eime.initCause(e);
			throw eime;
		}
		return result;
	}

}
