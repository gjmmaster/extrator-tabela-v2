(ns processador-crm.core
  (:require [dk.ative.docjure.spreadsheet :as ss]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;; --- ETAPA 1: CONFIGURAÇÃO ---
(def arquivo-entrada "base_teste.xlsx")
(def arquivo-saida "saida_para_crsm_inferida.csv")

(def mapa-modelos
  {"strada" "Strada", "onix" "Onix", "gol" "Gol", "hb20" "HB20",
   "compass" "Compass", "kwid" "Kwid", "t-cross" "T-Cross", "jeep" "Jeep",
   "fiat" "Fiat", "chevrolet" "Chevrolet", "vw" "VW", "renault" "Renault"})

;; --- ETAPA 2: FUNÇÕES DE TRANSFORMAÇÃO ---

(defn extrair-primeiro-nome [nome-completo]
  (let [s (str nome-completo)]
    (when-not (str/blank? s)
      (first (str/split s #"\s+")))))

(defn padronizar-telefone [telefone-str]
  (let [s (str telefone-str)]
    (when-not (str/blank? s)
      (str/replace s #"[^\d]" ""))))

(defn simplificar-modelo [descricao]
  (let [descricao-lower (some-> (str descricao) str/lower-case)]
    (if descricao-lower
      (or (some (fn [[chave valor]] (when (str/includes? descricao-lower chave) valor))
                mapa-modelos)
          "Modelo não identificado")
      nil)))

;; --- ETAPA 3: OS DETETIVES (HEURÍSTICAS) ---

(defn parece-telefone? [valor]
  (let [limpo (padronizar-telefone valor)]
    (and limpo (<= 10 (count limpo) 13))))

(defn parece-nome? [valor]
  (let [s (str/trim (str valor))]
    (and (not (str/blank? s))
         (re-matches #"^[\p{L}\s\.]+$" s)
         (> (count s) 3))))

(defn contem-keyword-carro? [valor]
  (let [descricao-lower (some-> valor str/lower-case)]
    (and descricao-lower
         (some #(str/includes? descricao-lower (key %)) mapa-modelos))))

(defn score-coluna [funcao-detetive coluna]
  (let [amostra (take 50 coluna)
        acertos (count (filter funcao-detetive amostra))]
    (if (empty? amostra)
      0.0
      (/ acertos (double (count amostra))))))

(defn identificar-indices-auto [colunas]
  (let [scores (for [coluna colunas]
                 {:telefone (score-coluna parece-telefone? coluna)
                  :nome (score-coluna parece-nome? coluna)
                  :carro (score-coluna contem-keyword-carro? coluna)})
        find-best-idx (fn [tipo]
                        (->> scores
                             (map-indexed vector)
                             (apply max-key #(get-in % [1 tipo]))
                             first))]
    {:telefone (find-best-idx :telefone)
     :primeiro-nome (find-best-idx :nome)
     :modelo-carro (find-best-idx :carro)}))

(defn solicitar-indices-manual []
  (println "\n--- Modo Manual ---")
  (println "Por favor, insira o número da coluna (começando em 0) para cada campo:")
  (let [ler-indice (fn [campo]
                     (print (str campo ": "))
                     (flush)
                     (Integer/parseInt (read-line)))]
    {:telefone (ler-indice "Telefone")
     :primeiro-nome (ler-indice "Primeiro Nome")
     :modelo-carro (ler-indice "Modelo do Carro")}))

;; --- ETAPA 4: LÓGICA PRINCIPAL CORRIGIDA ---

(defn -main [& args]
  (try
    (println "Bem-vindo ao Processador de CRM!")
    (println "A sua planilha contém uma linha de cabeçalho? (s/n)")
    (print "Opção: ")
    (flush)
    (let [tem-cabecalho? (= (str/lower-case (read-line)) "s")
          ;; Carrega o workbook e extrai os dados da primeira planilha
          todos-os-dados (let [workbook (ss/load-workbook arquivo-entrada)]
                           (->> (first (ss/sheet-seq workbook))
                                (ss/row-seq)
                                (map (fn [row] (->> (ss/cell-seq row) (map ss/read-cell) doall)))
                                doall))
          dados-brutos (if tem-cabecalho? (rest todos-os-dados) todos-os-dados)]

      (if (empty? dados-brutos)
        (println "\nA planilha está vazia ou contém apenas o cabeçalho. Nenhum dado para processar.")
        (do
          (println "\nEscolha o modo de operação:")
          (println "1. Automático (detectar colunas)")
          (println "2. Manual (informar colunas)")
          (print "Opção: ")
          (flush)

          (let [modo (read-line)
                colunas (apply mapv vector dados-brutos)
                indices (case modo
                          "1" (let [indices-auto (identificar-indices-auto colunas)]
                                (println (str "\nÍndices inferidos automaticamente: " indices-auto))
                                indices-auto)
                          "2" (solicitar-indices-manual)
                          (do (println "Opção inválida. Usando modo automático por padrão.")
                              (let [indices-auto (identificar-indices-auto colunas)]
                                (println (str "\nÍndices inferidos automaticamente: " indices-auto))
                                indices-auto)))]

            (println (str "Índices a serem utilizados: " indices))

            (let [dados-processados (map (fn [linha]
                                           {:telefone (padronizar-telefone (get linha (:telefone indices)))
                                            :primeiro-nome (extrair-primeiro-nome (get linha (:primeiro-nome indices)))
                                            :modelo-carro (simplificar-modelo (get linha (:modelo-carro indices)))})
                                         dados-brutos)
                  cabecalho [:telefone :primeiro-nome :modelo-carro]
                  linhas (map (apply juxt cabecalho) dados-processados)]

              (with-open [writer (io/writer arquivo-saida)]
                (csv/write-csv writer linhas))

              (println (str "\nArquivo \"" arquivo-saida "\" criado com sucesso!")))))))

    (catch java.io.FileNotFoundException e
      (println (str "\nERRO: Arquivo de entrada \"" arquivo-entrada "\" não encontrado.")))
    (catch Exception e
      (println (str "\nOcorreu um erro inesperado: " (.getMessage e)))
      (.printStackTrace e))))