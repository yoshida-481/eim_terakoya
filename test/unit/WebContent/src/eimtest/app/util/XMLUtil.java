package eimtest.app.util;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.Assert;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.NodeIterator;
import org.xml.sax.InputSource;

/** */
public class XMLUtil {
	/** */
	DocumentBuilder builder;

	/** */
	Transformer transformer;

	/** */
	public XMLUtil() {
		try
		{
			DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
			dbfactory.setIgnoringElementContentWhitespace(false);
			builder = dbfactory.newDocumentBuilder();

			TransformerFactory factory = TransformerFactory.newInstance();
			transformer = factory.newTransformer();
		} catch (Exception e)
		{
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * @param xml
	 * @return o
	 * @throws Exception
	 */
	public Document toDOM(String xml) throws Exception
	{
		Document doc = builder.parse(new InputSource(new StringReader(xml)));
		return doc;
	}

	/**
	 * 
	 * @param xml
	 * @return o
	 * @throws Exception
	 */
	public String toStr(Node xml) throws Exception
	{
		StringWriter ret = new StringWriter();
		transformer.transform(new DOMSource(xml), new StreamResult(ret));
		return ret.toString();
	}

	/**
	 * 
	 * @param expects
	 * @param node
	 * @param xpath
	 * @throws TransformerException
	 */
	public void assertNodeValues(String[] expects, Node node, String xpath)
			throws TransformerException
	{
		NodeIterator ni = XPathAPI.selectNodeIterator(node, xpath);
		List result = new ArrayList();
		for (Node n; (n = ni.nextNode()) != null;)
		{
			result.add(n.getNodeValue());
		}
		Assert.assertEquals(Arrays.asList(expects).toString(), result.toString());
	}
	
	/**
	 * 
	 * @return o
	 */
	public Document createDocument() {
		return builder.newDocument();
	}
}
