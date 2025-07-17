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
   "fiat" "Fiat", "chevrolet" "Chevrolet", "vw" "VW", "renault" "Renault",
   "montana" "Montana", "cruze" "Cruze", "spin" "Spin", "tracker" "Tracker"})

;; --- ETAPA 2: FUNÇÕES DE TRANSFORMAÇÃO ---

(defn extrair-primeiro-nome [nome-completo]
  (let [s (str nome-completo)]
    (when-not (str/blank? s)
      (let [partes (str/split s #"\s+")]
        (if (seq partes)
          (first partes)
          s)))))

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
         ;; Verificar se contém pelo menos uma letra
         (re-find #"[a-zA-Z]" s)
         ;; Não deve começar com +, (, ou dígitos (para evitar telefones)
         (not (re-matches #"^[\+\(\d].*" s))
         ;; Deve ter pelo menos 3 caracteres
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
                             first))
        ;; Para nome, vamos priorizar colunas mais à esquerda (geralmente o nome do cliente vem antes)
        find-best-nome-idx (fn []
                             (->> scores
                                  (map-indexed vector)
                                  (filter #(> (get-in % [1 :nome]) 0.5)) ; Score mínimo para ser considerado nome
                                  (sort-by first) ; Ordena por índice (prioriza colunas à esquerda)
                                  first
                                  first))]
    {:telefone (find-best-idx :telefone)
     :primeiro-nome (or (find-best-nome-idx) (find-best-idx :nome))
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

      ;; DEBUG: Mostrar estrutura dos dados
      (println "\n=== DEBUG: Estrutura dos dados ===")
      (println "Total de linhas (incluindo cabeçalho):" (count todos-os-dados))
      (println "Primeira linha (cabeçalho):")
      (println (first todos-os-dados))
      (println "Segunda linha (primeiro registro):")
      (println (second todos-os-dados))
      (println "Número de colunas:" (count (first todos-os-dados)))
      (println "=== FIM DEBUG ===\n")

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

            ;; Processar os dados linha por linha
            (let [linhas-processadas (for [linha dados-brutos]
                                       (let [telefone-raw (nth linha (:telefone indices) nil)
                                             nome-raw (nth linha (:primeiro-nome indices) nil)
                                             modelo-raw (nth linha (:modelo-carro indices) nil)
                                             
                                             ;; Extrair apenas o primeiro nome
                                             nome-completo (str nome-raw)
                                             primeiro-nome (if (str/blank? nome-completo)
                                                            ""
                                                            (first (str/split nome-completo #"\s+")))
                                             
                                             ;; Processar telefone e modelo
                                             telefone (padronizar-telefone telefone-raw)
                                             modelo (simplificar-modelo modelo-raw)]
                                         
                                         ;; DEBUG: Mostrar processamento das primeiras 3 linhas
                                         (when (< (.indexOf dados-brutos linha) 3)
                                           (println (str "Linha " (.indexOf dados-brutos linha) ":"))
                                           (println (str "  Nome completo: " nome-completo))
                                           (println (str "  Primeiro nome: " primeiro-nome))
                                           (println (str "  Telefone: " telefone))
                                           (println (str "  Modelo: " modelo)))
                                         
                                         ;; Retornar a linha processada
                                         [telefone primeiro-nome modelo]))]

              ;; Escrever o arquivo CSV
              (with-open [writer (io/writer arquivo-saida)]
                (csv/write-csv writer linhas-processadas))

              (println (str "\nArquivo \"" arquivo-saida "\" criado com sucesso!")))))))

    (catch java.io.FileNotFoundException e
      (println (str "\nERRO: Arquivo de entrada \"" arquivo-entrada "\" não encontrado.")))
    (catch Exception e
      (println (str "\nOcorreu um erro inesperado: " (.getMessage e)))
      (.printStackTrace e))))