import scala.io.StdIn.readLine;
import scala.io.Source;
import java.io.FileNotFoundException;
import java.nio.file.Paths;

object Main extends App {
  // "Banco de dados" da aplicação
  var data: List[(String, Integer)] = List();
  var frequencies = scala.collection.mutable.Map[String, Integer]();
  var index = 0;

  val stopWords = Set("a","able","about","across","after","all",
                      "almost","also","am","among","an","and","any",
                      "are","as","at","be","because","been","but","by",
                      "can","cannot","could","dear","did","do","does",
                      "either","else","ever","every","for","from","get",
                      "got","had","has","have","he","her","hers","him",
                      "his","how","however","i","if","in","into","is","it",
                      "its","just","least","let","like","likely","may","me",
                      "might","most","must","my","neither","no","nor","not",
                      "of","off","often","on","only","or","other","our","own",
                      "rather","said","say","says","she","should","since","so",
                      "some","than","that","the","their","them","then","there",
                      "these","they","this","tis","to","too","twas","us","wants",
                      "was","we","were","what","when","where","which","while",
                      "who","whom","why","will","with","would","yet","you","your");

  // Funções do lado do "servidor"
  def error_state(): (String, Map[String, Array[String]]) = {
    return ("Something wrong", Map("0" -> Array("get", "default", "None")));
  }

  def default_get_handler(): (String, Map[String, Array[String]]) = {
    var rep = "What would you like to do?";
    rep += "\n1 - Quit" + "\n2 - Upload file";
    var links = Map("1" -> Array("post", "execution", "None"), "2" -> Array("get", "file_form", "None"))
    return (rep, links)
  }

  def quit_handler(): (String, Map[String, Array[String]]) = {
    println("Goodbye cruel world...");
    System.exit(0);

    return ("", Map("" -> Array("")));
  }

  def upload_get_handler(): (String, Map[String, Array[String]]) = {
    return ("Type '0' and next type the name of file to upload.", Map("0" -> Array("post", "file", "None")));
  }

  def upload_post_handler(path: String): (String, Map[String, Array[String]]) = {;
    index = 0;
    frequencies = scala.collection.mutable.Map[String, Integer]();
    def create_data(path: String) = {
      val bufferedSource = Source.fromFile(Paths.get(path).toAbsolutePath().toString());
      for (line <- bufferedSource.getLines) {
        var processedLine = line.split(" ").map(w => w.replaceAll("[^a-zA-Z]", "").toLowerCase).toList;
        for (word <- processedLine) {
          if (word.length >= 1 && !stopWords.contains(word)) {
            val current : Integer = frequencies.getOrElse(word, 0);
            frequencies(word) = current + 1;
          }
        }
      }

      data = frequencies.toList.sortBy(_._2).reverse;

      bufferedSource.close;
    }

    if (path == "None") {
      return error_state();
    }
    try {
      create_data(path);
    } catch {
      case e: FileNotFoundException => {
        println("Unexpected error");
        return quit_handler();
      }
    }

    return word_get_handler();
  }

  // word get handler
  def word_get_handler(): (String, Map[String, Array[String]]) = {
    var rep = data(index)._1 + " " + frequencies(data(index)._1);
    index += 1;
    rep += "\n\n What would you like to do next?";
    rep += "\n1 - Quit" + "\n2 - Upload file";
    rep += "\n3 - See next most-frequently occurring word"
    var links = Map("1" -> Array("post", "execution", "None"),
                "2" -> Array("get", "file_form", "None"),
                "3" -> Array("get", "word", "None")
              );
    return (rep, links);
  }

  // Representação do nosso "servidor"                  
  def handle_request(verb: String, uri: String, args: String): (String, Map[String, Array[String]]) = {
    def handler_key(verb: String, uri: String): String = {
      return verb + "_" + uri
    }
    
    var method = handler_key(verb, uri);

    method match {
      case "get_default" => {
       return default_get_handler();
      }
      case "post_execution" => {
        return quit_handler();
      }
      case "get_file_form" => {
        return upload_get_handler();
      }
      case "post_file" => {
        return upload_post_handler(args);
      }
      case "get_word" => {
        return word_get_handler();
      }
    }

    return null;
  }

  // Representação do lado do "cliente"
  def render_and_get_input(state_representation: String, links: Map[String, Array[String]]): Array[String] = {
    println(state_representation)
    Console.flush()
    
    var input = readLine();

    if (input == "0") {
      var array = links("0");
      if(array(0) == "post") {
        var textInput = readLine();
        array(2) = textInput;
        return array;
      } else {
        return array;
      }
    } else {
      if (links.contains(input)) {
        return links(input)
      } else {
        return Array("get", "default", "None")
      }
    }

    return null;
  }

  var request = Array("get", "default", "None") : Array[String];

  while (true) {
    // Servidor envia uma resposta
    var response = handle_request(request(0), request(1), request(2));
    var state_representation = response._1;
    var links = response._2;
    
    // Cliente envia uma requisição
    request = render_and_get_input(state_representation, links);
  }
}