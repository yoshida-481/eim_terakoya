package common.util;

import jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain;
import jp.co.ctc_g.eim.admin.business.service.NamespaceService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;

import org.springframework.context.ApplicationContext;

import eim.util.EIMConfig;

public class NamespaceUtil {

	/**
	 * ネームスペース括弧付の定義名称文字列の作成
	 * 定義名称が未設定or空文字などの場合はnameを返却する。
	 * @param Name    他言語名称
	 * @param defName 定義名称
	 * @return ネームスペース括弧付の他言語名称
	 */
	public static String getDefNameWithNamespaceParentheses(String name, String defName) throws Exception {
		
		String retName = "";
		
		// 定義名称が設定されていた場合
		if(defName != null && defName.length() > 0) {
			
			// ネームスペースが設定されていた場合
			if (defName.indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR")) != -1) {
				
				ApplicationContext context = ApplicationContextLoader.getContext();
				NamespaceService namespaceService = (NamespaceService)context.getBean("NamespaceService");
	
				String namespace = defName.substring(0, defName.indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR")));
				NamespaceDomain namespaceDomain = namespaceService.getByNamespaceName(namespace);
				// ネームスペースを除いた定義名称を取得
				String tmpName = defName.substring(defName.indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR")) + 1);
	
				// ネームスペースが存在した場合
				if (namespaceDomain != null) {
					
					// ネームスペースを除いた定義名称が存在すれば括弧付きで返却
					if (tmpName != null &&  tmpName.length() > 0) {
						
						retName = tmpName + "(" + namespace + ")";
						
					// 他言語名称が存在すれば括弧付きで返却
					} else if (name != null && name.length() > 0) {
						
						retName = name + "(" + namespace + ")";
					}
					
				// ネームスペースが存在しない場合
				} else {
					
					// ネームスペースを除いた定義名称が存在した場合
					if (tmpName != null &&  tmpName.length() > 0) {
						
						// ネームスペースを除いた定義名称を返却
						retName = tmpName;
						
					// 他言語名称が存在した場合
					} else if (name != null && name.length() > 0) {
						
						// 他言語名称をそのまま返却
						retName = name;
					}
				}
				
			// ネームスペースが設定されていない場合
			} else {
				
				// 定義名称をそのまま返却
				retName = defName;
			}
			
		// 他言語名称が設定されていた場合
		} else if (name != null && name.length() > 0) {
			
			// 他言語名称をそのまま返却
			retName = name;
		}
		
		return retName;
	}

	/**
	 * 定義名称からネームスペースを取得する。
	 *
	 * @param DefName 定義名称
	 * @return ネームスペース
	 */
	public static String getNamespaceByDefName(String defName){

		if (defName != null && !defName.equals("") && defName.indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR")) != -1)
		{
			return defName.substring(0, defName.indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR")));
		}
		else
		{
			return "";
		}
	}


	/**
	 * 定義名称からネームスペースを除いた名称を取得する。<br>
	 * defNameにネームスペースが存在しない場合は、引数defNameをそのまま(nullの場合は空文字)返却する。<br>
	 *
	 * @param defName 定義名称
	 * @return ネームスペースを除いた名称
	 */
	public static String getDefNamenWhichExceptedNamespace(String defName){

		String retDefName = "";

		if (defName != null && defName.length() != 0) {

			// デリミタ文字列の位置を取得
			int index = defName.indexOf(EIMConfig.get("NAMESPACE_DIVID_CHAR"));

			// デリミタ文字列の位置を取得できた場合
			if (index != -1) {

				// 定義名称からネームスペースを除いた名称を取得
				retDefName = defName.substring(index+1, defName.length());

			// デリミタ文字列が存在しない場合
			} else {

				// 引数DefNameを返却
				retDefName = defName;
			}
		}

		return retDefName;
	}
	
	/**
	 * ネームスペースと名称を連結して返却する。<br>
	 * ネームスペースと名称の間には区切り文字を挿入する。<br>
	 * ネームスペースまたは名称がNULLもしくは空文字の場合、区切り文字を挿入せずに返却する。
	 * @param namespace    他言語名称
	 * @param name 定義名称
	 * @return ネームスペースと名称が連結された文字列(定義名称)
	 */
	public static String concatenate(String namespace, String name) throws Exception {
		
		String retName = "";
		
		if (namespace != null && namespace.length() > 0) {
			retName = retName + namespace;
			if (name != null && name.length() > 0) {
				retName = retName + EIMConfig.get("NAMESPACE_DIVID_CHAR") + name;
			}
		}
		else {
			if (name != null && name.length() > 0) {
				retName = retName + name;
			}
		}
		
		return retName;
	}
}
