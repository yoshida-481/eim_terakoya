package eimtest.app.util;

import java.io.FileNotFoundException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ClassUtils;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.NodeIterator;

import eim.bo.EIMObject;
import eimtest.util.Misc;

/** */
public class TestAppMisc {
	/**
	 * 
	 * @param o
	 * @param postfix
	 * @return o
	 * @throws Exception
	 */
	public static String loadAppTextFile(Object o, String postfix) throws Exception {
		return Misc.loadFile(getAppFilePath(o.getClass(), postfix));
	}

	/**
	 * 
	 * @param c
	 * @param postfix
	 * @return o
	 * @throws Exception
	 */
	public static String getAppFilePath(Class c, String postfix) throws Exception {
		String fileName = ClassUtils.getShortClassName(c) + postfix;
		URL url = c.getResource(fileName);
		if (url == null)
			throw new FileNotFoundException(fileName);
		return url.getPath();
	}

	/**
	 * 
	 * @param assertText
	 * @param objs
	 * @return o
	 * @throws Exception
	 */
	public static String replaceAssertText(String assertText, List objs) throws Exception {
		if (objs != null) {
			for (Iterator i = objs.iterator(); i.hasNext();) {
				EIMObject obj = (EIMObject) i.next();
				assertText = assertText.replaceAll("\\[id\\_" + obj.getName() + "\\]",
					String.valueOf(obj.getId()))//
				.replaceAll("\\[tid\\_" + obj.getName() + "\\]",
					String.valueOf(obj.getType().getId()))//
				;
			}
		}
		return assertText;
	}

	/**
	 * 
	 * @param doc
	 * @param xpath
	 * @return o
	 * @throws Exception
	 */
	public static List getNodeValues(Document doc, String xpath) throws Exception {
		List objIds = new ArrayList();
		NodeIterator nl = XPathAPI.selectNodeIterator(doc, xpath);
		for (Node n; (n = nl.nextNode()) != null;) {
			objIds.add(n.getNodeValue());
		}
		return objIds;
	}

	/**
	 * 
	 * @param m
	 * @return o
	 */
	public static String mapToString(Map m) {
		StringBuffer sb = new StringBuffer();
		for (Iterator i = m.keySet().iterator(); i.hasNext();) {
			Object key = i.next();
			sb.append(key.toString() + m.get(key) + "\n");
		}
		return sb.toString();
	}
}
