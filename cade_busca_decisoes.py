import requests
from bs4 import BeautifulSoup
from googlesearch import search
import os

# Função para fazer a busca no Google
def search_certidoes(query, num_results=10):
    results = []
    for result in search(query, num_results=num_results):
        if "sei.cade.gov.br" in result:
            results.append(result)
    return results

# Função para baixar o conteúdo HTML das certidões
def download_html(certidao_urls, save_folder="certidoes_html"):
    if not os.path.exists(save_folder):
        os.makedirs(save_folder)
        
    for url in certidao_urls:
        try:
            response = requests.get(url)
            if response.status_code == 200:
                soup = BeautifulSoup(response.content, 'html.parser')
                title = soup.title.string.strip().replace(" ", "_").replace("/", "_")
                file_path = os.path.join(save_folder, f"{title}.html")
                
                # Salvando o HTML
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(str(soup))
                print(f"Downloaded: {title}")
            else:
                print(f"Failed to download: {url} - Status code: {response.status_code}")
        except Exception as e:
            print(f"Error downloading {url}: {e}")

# Configuração da busca
search_query = 'site:https://sei.cade.gov.br/sei/modulos/pesquisa/md_pesq_documento_consulta_externa.php? "CERTIDÃO DE JULGAMENTO"'

# Busca os links das certidões
certidao_links = search_certidoes(search_query, num_results=50)

# Baixa o conteúdo HTML de cada link
download_html(certidao_links)
